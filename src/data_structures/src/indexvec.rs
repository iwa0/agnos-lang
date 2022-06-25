use std::marker::PhantomData;
use std::num::NonZeroU32;
use std::ops::Deref;

#[macro_export]
macro_rules! new_index_type {
    ($element_name:ident, $slice_name:ident) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub struct $element_name($crate::indexvec::ElementIndex);

        //#[derive(Clone, Copy, Debug, PartialEq, Eq)]
        //struct $array_name<const N: usize>($crate::indexvec::ArrayIndex<N>);

        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub struct $slice_name($crate::indexvec::SliceIndex);

        impl std::ops::Deref for $element_name {
            type Target = $crate::indexvec::ElementIndex;
            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }
        /*impl<const N: usize> std::ops::Deref for $array_name<N> {
            type Target = $crate::indexvec::ArrayIndex<N>;
            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }*/
        impl std::ops::Deref for $slice_name {
            type Target = $crate::indexvec::SliceIndex;
            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl $crate::indexvec::Index for $element_name {
            //type Array<const N: usize> = $array_name<N>;
            type Slice = $slice_name;
            fn new_element(idx: $crate::indexvec::ElementIndex) -> Self {
                $element_name(idx)
            }
            /*fn new_array<const N: usize>(idx: $crate::indexvec::ArrayIndex<N>) -> Self::Array<N> {
                $array_name::<N>(idx)
            }*/
            fn new_slice(idx: $crate::indexvec::SliceIndex) -> Self::Slice {
                $slice_name(idx)
            }
        }
    };
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IndexVec<Idx: Index, T>(Vec<T>, PhantomData<Idx>);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ElementIndex(NonZeroU32);
//#[derive(Clone, Copy, Debug, PartialEq, Eq)]
//pub struct ArrayIndex<const N: usize>(NonZeroU32);
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SliceIndex(NonZeroU32, u32);

pub trait Index: Deref<Target = ElementIndex> {
    //type Array<const N: usize>: Deref<Target = ArrayIndex<N>>;
    type Slice: Deref<Target = SliceIndex>;
    fn new_element(idx: ElementIndex) -> Self;
    //fn new_array<const N: usize>(idx: ArrayIndex<N>) -> Self::Array<N>;
    fn new_slice(idx: SliceIndex) -> Self::Slice;
}

pub trait BasicIndex<T>: Copy {
    type Output: ?Sized;
    fn get(v: &[T], idx: Self) -> &Self::Output;
    fn get_mut(v: &mut [T], idx: Self) -> &mut Self::Output;
}

impl ElementIndex {
    pub fn to_slice(&self) -> SliceIndex {
        SliceIndex(self.0, 1)
    }
}

/*impl<const N: usize> ArrayIndex<N> {
    /*pub fn nth(&self, n: usize) -> ElementIndex {
        assert!(n < N);
        let id = self.0.get().wrapping_add(n as u32);
        ElementIndex(NonZeroU32::new(id).unwrap())
    }*/
    pub fn len(&self) -> usize {
        N as usize
    }
    pub fn to_slice(&self) -> SliceIndex {
        SliceIndex(self.0, N as u32)
    }
}*/

impl SliceIndex {
    /*pub fn nth(&self, n: usize) -> ElementIndex {
        assert!(n < self.len());
        let id = self.0.get().wrapping_add(n as u32);
        ElementIndex(NonZeroU32::new(id).unwrap())
    }*/
    pub fn empty() -> SliceIndex {
        SliceIndex(NonZeroU32::new(1).unwrap(), 0)
    }
    pub fn len(&self) -> usize {
        self.1 as usize
    }
}

impl<T> BasicIndex<T> for ElementIndex {
    type Output = T;
    fn get(v: &[T], idx: Self) -> &Self::Output {
        &v[idx.0.get().wrapping_sub(1) as usize]
    }
    fn get_mut(v: &mut [T], idx: Self) -> &mut Self::Output {
        &mut v[idx.0.get().wrapping_sub(1) as usize]
    }
}

/*impl<T, const N: usize> BasicIndex<T> for ArrayIndex<N> {
    type Output = [T; N];
    fn get(v: &[T], idx: Self) -> &Self::Output {
        let start = idx.0.get().wrapping_sub(1) as usize;
        v.split_at(start).1.split_array_ref::<N>().0
    }
    fn get_mut(v: &mut [T], idx: Self) -> &mut Self::Output {
        let start = idx.0.get().wrapping_sub(1) as usize;
        v.split_at_mut(start).1.split_array_mut::<N>().0
    }
}*/

impl<T> BasicIndex<T> for SliceIndex {
    type Output = [T];
    fn get(v: &[T], idx: Self) -> &Self::Output {
        let start = idx.0.get().wrapping_sub(1) as usize;
        &v[start..start + idx.len()]
    }
    fn get_mut(v: &mut [T], idx: Self) -> &mut Self::Output {
        let start = idx.0.get().wrapping_sub(1) as usize;
        &mut v[start..start + idx.len()]
    }
}

impl<Idx: Index, T> IndexVec<Idx, T> {
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
    pub fn last_id(&self) -> Idx {
        let idx = self.index_as_nz32(1);
        Idx::new_element(ElementIndex(idx))
    }
    /*pub fn last_array_id<const N: usize>(&self) -> Idx::Array<N> {
        let idx = self.len_as_nz32(N);
        Idx::new_array(ArrayIndex(idx))
    }*/
    pub fn last_n_id(&self, n: usize) -> Idx::Slice {
        let idx = self.index_as_nz32(n);
        Idx::new_slice(SliceIndex(idx, n as u32))
    }
    pub fn push(&mut self, v: T) {
        self.0.push(v);
    }
    pub fn alloc(&mut self, v: T) -> Idx {
        self.push(v);
        self.last_id()
    }
    pub fn alloc_with<I: Iterator<Item = T>>(&mut self, it: I) -> Idx::Slice {
        let mut len = 0;
        for v in it {
            len += 1;
            self.push(v);
        }
        self.last_n_id(len)
    }

    pub fn element(&self, idx: Idx) -> &T {
        BasicIndex::get(&self.0, *idx)
    }
    /*pub fn array<const N: usize>(&self, idx: Idx::Array<N>) -> &[T; N] {
        BasicIndex::get(&self.0, *idx)
    }*/
    pub fn slice(&self, idx: Idx::Slice) -> &[T] {
        BasicIndex::get(&self.0, *idx)
    }

    pub fn element_mut(&mut self, idx: Idx) -> &mut T {
        BasicIndex::get_mut(&mut self.0, *idx)
    }
    /*pub fn array_mut<const N: usize>(&mut self, idx: Idx::Array<N>) -> &mut [T; N] {
        BasicIndex::get_mut(&mut self.0, *idx)
    }*/
    pub fn slice_mut(&mut self, idx: Idx::Slice) -> &mut [T] {
        BasicIndex::get_mut(&mut self.0, *idx)
    }
}

/*/
impl<Idx: Index, T, DerefIdx: Deref> std::ops::Index<DerefIdx> for IndexVec<Idx, T>
where
    <DerefIdx as Deref>::Target: BasicIndex<T>,
    //DerefIdx is Idx || Idx::Array<N> || Idx::Slice
{
    type Output = <<DerefIdx as Deref>::Target as BasicIndex<T>>::Output;
    fn index(&self, idx: DerefIdx) -> &Self::Output {
        BasicIndex::get(&self.0, *idx)
    }
}*/

/*
impl<Idx: Index, T> std::ops::Index<Idx> for IndexVec<Idx, T> {
    type Output = T;
    fn index(&self, idx: Idx) -> &Self::Output {
        BasicIndex::get(&self.0, *idx)
    }
}

impl<Idx: Index, T> std::ops::Index<Idx::Slice> for IndexVec<Idx, T>  {
    type Output = [T];
    fn index(&self, idx: Idx::Slice) -> &Self::Output {
        BasicIndex::get(&self.0, *idx)
    }
}
*/
