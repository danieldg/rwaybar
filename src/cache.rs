use crate::{
    state::{NotifierList, Runtime},
    util::{spawn_noerr, Cell},
};
use std::{
    future::Future,
    ops::Deref,
    pin::Pin,
    rc::Rc,
    task,
    time::{Duration, Instant},
};
use tokio::time::sleep_until;

#[derive(Copy, Clone, Debug)]
enum State {
    /// Initial state
    Empty,
    /// Value was produced at this timestamp, but it has not yet been read.
    ///
    /// A value in this state will not be refreshed.
    Ready {
        timestamp: Instant,
    },
    /// There is a task that will refresh the value when the cache expires.
    Refreshing,
    Abort,
}

#[derive(Debug)]
struct Inner<T> {
    period: f64,
    interested: NotifierList,
    state: Cell<State>,
    abort: Cell<Option<task::Waker>>,
    value: T,
}

#[derive(Debug)]
pub struct CachedValue<T>(Rc<Inner<T>>);

#[derive(Debug)]
pub struct Updater<T>(Rc<Inner<T>>);

impl<T> Updater<T> {
    pub fn notify_data(&self, reason: &str) {
        self.0.interested.notify_data(reason);
    }
}

impl<T> Deref for Updater<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.0.value
    }
}

pin_project_lite::pin_project! {
    struct Task<F, T> {
        #[pin]
        fut: F,
        inner: Rc<Inner<T>>,
    }
}

impl<T: 'static> CachedValue<T> {
    pub fn new(period: f64, value: T) -> Self {
        Self(Rc::new(Inner {
            period,
            value,
            interested: Default::default(),
            state: Cell::new(State::Empty),
            abort: Cell::new(None),
        }))
    }

    pub fn read_refresh<F, Fut>(&self, rt: &Runtime, mut refresh: F) -> &T
    where
        F: FnMut(Updater<T>) -> Fut + 'static,
        Fut: Future<Output = ()> + 'static,
    {
        let now = Instant::now();
        self.0.interested.add(rt);
        let mut state = self.0.state.get();
        if let State::Ready { timestamp } = state {
            if now.duration_since(timestamp).as_secs_f64() > self.0.period {
                state = State::Empty;
            }
        }

        if let State::Empty = state {
            let mut fut = Box::pin(refresh(Updater(self.0.clone())));
            let mut cx = task::Context::from_waker(task::Waker::noop());
            if fut.as_mut().poll(&mut cx).is_ready() {
                state = State::Ready { timestamp: now };
            } else {
                spawn_noerr(Task {
                    fut,
                    inner: self.0.clone(),
                });
                state = State::Refreshing;
            }
        }

        if let State::Ready { .. } = state {
            let deadline = (now + Duration::from_secs_f64(self.0.period)).into();
            let updater = Updater(self.0.clone());
            spawn_noerr(Task {
                fut: async move {
                    sleep_until(deadline).await;
                    refresh(updater).await
                },
                inner: self.0.clone(),
            });
            state = State::Refreshing;
        }

        self.0.state.set(state);
        &self.0.value
    }
}

impl<T> Drop for CachedValue<T> {
    fn drop(&mut self) {
        self.0.state.set(State::Abort);
        if let Some(waker) = self.0.abort.take() {
            waker.wake();
        }
    }
}

impl<F: Future<Output = ()>, T> Future for Task<F, T> {
    type Output = ();
    fn poll(self: Pin<&mut Self>, cx: &mut task::Context) -> task::Poll<()> {
        let this = self.project();
        if matches!(this.inner.state.get(), State::Abort) {
            return task::Poll::Ready(());
        }
        if this.fut.poll(cx).is_pending() {
            this.inner.abort.set(Some(cx.waker().clone()));
            return task::Poll::Pending;
        }
        this.inner.abort.set(None);
        this.inner.state.set(State::Ready {
            timestamp: Instant::now(),
        });
        task::Poll::Ready(())
    }
}
