/// A farm is where we keep our `Cow`s.
///
/// This is a vendored version of [`FrozenVec<Box<str>>`].
///
/// [`FrozenVec`]: https://docs.rs/elsa/latest/elsa/vec/struct.FrozenVec.html
#[derive(Default)]
pub struct Farm {
    inner: UnsafeCell<Vec<Box<str>>>,
}

impl Farm {
    pub fn add<'a>(&'a self, cow: CowStr<'a>) -> &'a str {
        let string = match cow {
            CowStr::Boxed(boxed) => boxed,
            CowStr::Borrowed(borrowed) => return borrowed,
            CowStr::Inlined(inline) => (*inline).into(),
        };
        let inner = unsafe { &mut *self.inner.get() };
        inner.push(string);

        // Invoking `Deref` for `Vec` is somewhat risky because of noalias. So we do pointer
        // shenanigans instead.
        unsafe { &*inner.as_mut_ptr().add(inner.len() - 1) }
    }
    pub fn clear(&mut self) {
        self.inner.get_mut().clear();
    }
}

use pulldown_cmark::CowStr;
use std::cell::UnsafeCell;
