use std::{
    fmt::{self, Debug},
    hash::Hash,
    marker::PhantomData,
    num::NonZeroU32,
    ops::Index,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IndexVec<Idx: IndexAccessor>(Vec<Idx::T>, PhantomData<Idx>);

pub struct ElementIndex<T>(ElementIndexInner, PhantomData<T>);

pub struct SliceIndex<T>(SliceIndexInner, PhantomData<T>);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct ElementIndexInner(NonZeroU32);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct SliceIndexInner(NonZeroU32, u32);

pub trait IndexAccessor {
    type T;
    type Output: ?Sized;
    fn get(slice: &[Self::T], idx: Self) -> &Self::Output;
    fn get_mut(slice: &mut [Self::T], idx: Self) -> &mut Self::Output;
}

impl<T> Clone for ElementIndex<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for ElementIndex<T> {}

impl<T> Hash for ElementIndex<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u32(self.0 .0.get())
    }
}

impl<T> PartialEq for ElementIndex<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for ElementIndex<T> {}

impl<T> Debug for ElementIndex<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("ElementIndex").field(&self.0).finish()
    }
}

impl<T> Clone for SliceIndex<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for SliceIndex<T> {}

impl<T> Hash for SliceIndex<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u32(self.0.0.get());
        state.write_u32(self.0.1);
    }
}

impl<T> PartialEq for SliceIndex<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for SliceIndex<T> {}

impl<T> Debug for SliceIndex<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("SliceIndex").field(&self.0).finish()
    }
}

#[derive(Clone, Copy, Debug /*PartialEq, Eq*/)]
pub struct SliceIter<T>(SliceIndex<T>);

impl ElementIndexInner {
    fn as_slice(self) -> SliceIndexInner {
        SliceIndexInner(self.0, 1)
    }
}

impl SliceIndexInner {
    fn empty() -> Self {
        Self(NonZeroU32::new(1).unwrap(), 0)
    }
    unsafe fn nth_unchecked(self, n: usize) -> ElementIndexInner {
        let idx = self.0.get().wrapping_add(n as _);
        let idx = NonZeroU32::new_unchecked(idx);
        ElementIndexInner(idx)
    }
    fn len(self) -> usize {
        self.1 as usize
    }
    unsafe fn trim_start_n_unchecked(self, n: usize) -> SliceIndexInner {
        let start = self.0.get().wrapping_add(n as _);
        let start = NonZeroU32::new_unchecked(start);
        let len = (self.1 as usize - n) as _;
        SliceIndexInner(start, len)
    }
}

impl<T> ElementIndex<T> {
    pub fn as_slice(self) -> SliceIndex<T> {
        SliceIndex(self.0.as_slice(), PhantomData)
    }
    pub fn index(self) -> usize {
        self.0 .0.get().wrapping_sub(1) as _
    }
}

impl<T> SliceIndex<T> {
    pub fn empty() -> Self {
        Self(SliceIndexInner::empty(), PhantomData)
    }
    pub unsafe fn nth_unchecked(self, n: usize) -> ElementIndex<T> {
        let idx = self.0.nth_unchecked(n);
        ElementIndex(idx, PhantomData)
    }
    pub fn nth(self, n: usize) -> ElementIndex<T> {
        assert!(n < self.len());
        unsafe { self.nth_unchecked(n) }
    }
    pub fn start_index(self) -> usize {
        self.0 .0.get().wrapping_sub(1) as usize
    }
    pub fn len(self) -> usize {
        self.0.len()
    }
    pub fn iter(self) -> SliceIter<T> {
        SliceIter(self)
    }
}

impl<T> Iterator for SliceIter<T> {
    type Item = ElementIndex<T>;
    fn next(&mut self) -> Option<Self::Item> {
        let inner = &mut self.0;
        if inner.0.len() > 0 {
            let item;
            let trimmed;
            unsafe {
                item = inner.nth_unchecked(0);
                trimmed = SliceIndex(inner.0.trim_start_n_unchecked(1), PhantomData);
            }
            *self = Self(trimmed);
            Some(item)
        } else {
            None
        }
    }
}

impl<T> IndexAccessor for ElementIndex<T> {
    type T = T;
    type Output = T;
    fn get(slice: &[Self::T], idx: Self) -> &Self::Output {
        &slice[idx.index()]
    }
    fn get_mut(slice: &mut [Self::T], idx: Self) -> &mut Self::Output {
        &mut slice[idx.index()]
    }
}

impl<T> IndexAccessor for SliceIndex<T> {
    type T = T;
    type Output = [T];
    fn get(slice: &[Self::T], idx: Self) -> &Self::Output {
        let start = idx.start_index();
        &slice[start..start + idx.len()]
    }
    fn get_mut(slice: &mut [Self::T], idx: Self) -> &mut Self::Output {
        let start = idx.start_index();
        &mut slice[start..start + idx.len()]
    }
}

impl<Idx: IndexAccessor> IndexVec<Idx> {
    pub fn new() -> Self {
        Self(Vec::new(), PhantomData)
    }
    fn index_as_nz32(&self, n: usize) -> NonZeroU32 {
        let len = self.len() - n + 1;
        NonZeroU32::new(len as u32).unwrap()
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn last_id(&self) -> ElementIndex<Idx::T> {
        let idx = self.index_as_nz32(1);
        let inner = ElementIndexInner(idx);
        ElementIndex(inner, PhantomData)
    }
    pub fn last_n_id(&self, n: usize) -> SliceIndex<Idx::T> {
        let idx = self.index_as_nz32(n);
        let inner = SliceIndexInner(idx, n as u32);
        SliceIndex(inner, PhantomData)
    }

    pub fn push(&mut self, v: Idx::T) {
        self.0.push(v);
    }
    pub fn alloc(&mut self, v: Idx::T) -> ElementIndex<Idx::T> {
        self.push(v);
        self.last_id()
    }
    pub fn alloc_with<I: Iterator<Item = Idx::T>>(&mut self, it: I) -> SliceIndex<Idx::T> {
        let mut len = 0;
        for v in it {
            self.push(v);
            len += 1;
        }
        self.last_n_id(len)
    }
}

impl<I: IndexAccessor, II: IndexAccessor<T = I::T>> Index<II> for IndexVec<I> {
    type Output = II::Output;
    fn index(&self, index: II) -> &Self::Output {
        II::get(self.0.as_slice(), index)
    }
}
