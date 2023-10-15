use futures_util::future::RemoteHandle;
use futures_util::FutureExt;
use log::error;
use std::borrow::Cow;
use std::convert::Infallible;
use std::error::Error;
use std::fmt;
use std::fs;
use std::future::Future;
use std::os::unix::io::AsRawFd;
use std::os::unix::io::RawFd;
use std::path::PathBuf;

pub fn toml_to_string(value: Option<&toml::Value>) -> Option<String> {
    value.and_then(|value| {
        if let Some(v) = value.as_str() {
            Some(v.to_owned())
        } else if let Some(v) = value.as_integer() {
            Some(format!("{}", v))
        } else if let Some(v) = value.as_float() {
            Some(format!("{}", v))
        } else {
            None
        }
    })
}

pub fn toml_to_f64(value: Option<&toml::Value>) -> Option<f64> {
    value.and_then(|value| {
        if let Some(v) = value.as_float() {
            Some(v)
        } else if let Some(v) = value.as_integer() {
            Some(v as f64)
        } else {
            None
        }
    })
}

#[derive(Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct ImplDebug<T>(pub T);

impl<T> fmt::Debug for ImplDebug<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}", std::any::type_name::<T>())
    }
}

impl<T> From<T> for ImplDebug<T> {
    fn from(t: T) -> Self {
        Self(t)
    }
}

impl<T> std::ops::Deref for ImplDebug<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T> std::ops::DerefMut for ImplDebug<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.0
    }
}

/// Wrapper around [std::cell::Cell] that implements [fmt::Debug] and has a few more useful utility
/// funcitons.
#[derive(Default)]
pub struct Cell<T>(std::cell::Cell<T>);

impl<T> Cell<T> {
    pub fn new(t: T) -> Self {
        Cell(std::cell::Cell::new(t))
    }

    #[allow(dead_code)]
    pub fn into_inner(self) -> T {
        self.0.into_inner()
    }
}

impl<T: Default> Cell<T> {
    pub fn take_in<F: FnOnce(&mut T) -> R, R>(&self, f: F) -> R {
        let mut t = self.0.take();
        let rv = f(&mut t);
        self.0.set(t);
        rv
    }
}

impl<T> Cell<Option<T>> {
    pub fn take_in_some<F: FnOnce(&mut T) -> R, R>(&self, f: F) -> Option<R> {
        let mut t = self.0.take();
        let rv = t.as_mut().map(f);
        self.0.set(t);
        rv
    }
}

impl<T> std::ops::Deref for Cell<T> {
    type Target = std::cell::Cell<T>;
    fn deref(&self) -> &std::cell::Cell<T> {
        &self.0
    }
}

impl<T> fmt::Debug for Cell<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "Cell")
    }
}

/// A simple wrapper around [RawFd] that implements [AsRawFd].
///
/// Note: it does nothing on drop; the file descriptor lifetime must be managed elsewhere.
#[derive(Debug)]
pub struct Fd(pub RawFd);
impl AsRawFd for Fd {
    fn as_raw_fd(&self) -> RawFd {
        self.0
    }
}

pub fn spawn_noerr(fut: impl Future<Output = ()> + 'static) {
    tokio::task::spawn_local(fut);
}

pub fn spawn(owner: &'static str, fut: impl Future<Output = Result<(), Box<dyn Error>>> + 'static) {
    spawn_noerr(async move {
        match fut.await {
            Ok(()) => {}
            Err(e) => {
                error!("{}: {}", owner, e);
            }
        }
    });
}

pub fn spawn_critical(
    owner: &'static str,
    fut: impl Future<Output = Result<Infallible, Box<dyn Error>>> + 'static,
) {
    spawn_noerr(async move {
        match fut.await {
            Ok(i) => match i {},
            Err(e) => {
                error!("{}: {}", owner, e);
                std::process::exit(0);
            }
        }
    });
}

pub fn spawn_handle(
    owner: &'static str,
    fut: impl Future<Output = Result<(), Box<dyn Error>>> + 'static,
) -> RemoteHandle<()> {
    let (task, rh) = async move {
        match fut.await {
            Ok(()) => {}
            Err(e) => {
                error!("{}: {}", owner, e);
            }
        }
    }
    .remote_handle();
    spawn_noerr(task);
    rh
}

pub fn glob_expand<'a>(file: impl Into<Cow<'a, str>>) -> Option<(Cow<'a, str>, bool)> {
    let file = file.into();
    if !file.contains('*') {
        return Some((file, false));
    }

    let mut candidates;
    let components;
    if file.starts_with("/") {
        candidates = vec![PathBuf::from("/")];
        components = file[1..].split('/');
    } else {
        candidates = vec![PathBuf::from(".")];
        components = file.split('/');
    }
    for component in components {
        if component.contains('*') {
            let mut re = String::new();
            for (i, chunk) in component.split('*').enumerate() {
                if i == 0 {
                    re.push('^');
                } else {
                    re.push_str(".*");
                }
                re.push_str(&regex::escape(chunk));
            }
            re.push('$');
            let re = regex::Regex::new(&re).expect("Invalid regex");
            candidates = candidates
                .into_iter()
                .filter_map(|c| fs::read_dir(c).ok())
                .flatten()
                .filter_map(Result::ok)
                .filter(|e| match e.file_name().to_str() {
                    Some(name) => re.is_match(name),
                    None => false,
                })
                .map(|e| e.path())
                .collect();
        } else {
            for p in &mut candidates {
                p.push(component);
            }
        }
    }
    let mut candidates = candidates
        .into_iter()
        .filter_map(|p| p.into_os_string().into_string().ok());

    let c = candidates.next()?;
    Some((c.into(), candidates.next().is_some()))
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct UID(u64);

impl UID {
    pub fn new() -> Self {
        use std::sync::atomic::{AtomicU64, Ordering};
        static N: AtomicU64 = AtomicU64::new(0);
        Self(N.fetch_add(1, Ordering::Relaxed))
    }
}
