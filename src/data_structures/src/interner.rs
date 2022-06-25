use std::{
    collections::{HashMap},
    num::NonZeroU32,
};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Intern(NonZeroU32);

impl Intern {
    pub fn index(self) -> u32 {
        self.0.get()
    }
}

#[derive(Clone)]
pub struct Interner {
    pool: Vec<u32>, // {len: u32, bytes: [u8; len], padding: [u8; _]}, ...
    //interns: HashMap<Intern, ()>,
    interns: HashMap<Box<[u8]>, Intern>,
}

impl Interner {
    pub fn new() -> Self {
        Self {
            pool: Vec::new(),
            interns: HashMap::new(),
        }
    }
    pub fn intern(&mut self, s: &[u8]) -> Intern {
        if let Some(&s) = self.interns.get(s) {
            s
        } else {
            let id = Self::pool_put(&mut self.pool, s);
            self.interns.insert(<Box<[u8]>>::from(s), id);
            id
        }
    }
    pub fn get(&self, id: Intern) -> &[u8] {
        Self::pool_get(&self.pool, id)
    }
    fn pool_put(pool: &mut Vec<u32>, s: &[u8]) -> Intern {
        let old_len = pool.len();
        let words = ((s.len() + 3) >> 2) + 1;
        pool.reserve(words);
        unsafe {
            /*
            let data = pool.spare_capacity_mut();
            data[words - 1].write(0);
            data[0].write(s.len() as u32);
            s.as_ptr().copy_to(data[1..].as_ptr() as *mut u8, s.len());
            */
            pool.set_len(old_len + words);

            let buf = pool.as_mut_ptr().add(old_len);
            buf.add(words - 1).write(0);
            buf.write(s.len() as u32);
            s.as_ptr().copy_to(buf.add(1) as *mut u8, s.len());
            
            Intern(NonZeroU32::new_unchecked((old_len + 1) as u32))
        }
    }
    fn pool_get(pool: &Vec<u32>, id: Intern) -> &[u8] {
        let data_idx = id.index() as usize;
        let len = pool[data_idx - 1] as usize;
        let words = (len + 3) >> 2;
        let data = (&pool[data_idx..data_idx + words]).as_ptr() as *const u8;
        unsafe { std::slice::from_raw_parts(data, len) }
    }
}
