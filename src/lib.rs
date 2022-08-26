use std::ops::{Add, Sub, Mul, AddAssign, SubAssign, MulAssign, ShlAssign, ShrAssign, Deref, DerefMut};

//TODO: u64 can be replaced with a generic type
#[derive(Debug, Clone)]
struct Vln {
    value: Vec<u64>
}

#[allow(dead_code)]
impl Vln {

    /// if 'vals = [a0, a1, a2,...an]' then the resulting Vln represents the
    /// value 'a0 + a1*(2^64) + a2 * (2^(2*64)) + ... + an * (2^(n*64))'
    pub fn new<T: Into<u64> + Clone>(vals: &[T]) -> Self {
        vals.into()
    }

    pub fn with_digits(amnt: usize) -> Self {
        let mut temp = Vec::new();
        temp.resize(amnt, 0u64);
        temp.as_slice().into()
    }

    pub fn digits(&self) -> usize {
        self.value.len()
    }

    pub fn to_hex_str(&self) -> String {
        let mut rep = String::new();
        self.value.iter().rev().for_each(|base_64| rep.push_str(&format!("{:x}", base_64)));
        rep
    }

    //TODO
    // pub fn to_dec_str(&self) -> String {
    //     let mut rep = String::new();
    //     // let mut carry = 0;
    //     // for digit in 0..self.value.len() {
    //     //     for hex_idx in 0..16 {
    //     //         let hex_digit = TryInto::<u8>::try_into((self.value[digit] & (0xf << hex_idx)) >> hex_idx).expect("");
    //     //         carry += hex_digit;
    //     //         rep.push(1 % 9 + 0x30);
    //     //     }
    //     // }
    //     rep
    // }

    //* The 'indexed_*' functions will be called alot, can it be faster? */
    fn indexed_add(&mut self, mut idx: usize, val: u64) {
        let mut over;
        if idx >= self.len() {self.value.resize(idx + 1, 0);}
        (self[idx], over) = self[idx].overflowing_add(val.into());
        while over {
            idx += 1;
            if idx == self.len() {self.value.push(0);}
            (self[idx], over) = self[idx].overflowing_add(1);
        }
    }

    /// does subtraction, panics if the subtraction would cause overflow
    /// If the function panics 'self' will be modified, as though the subtraction wrapped at the current size of 'self'
    fn indexed_sub(&mut self, mut idx: usize, val: u64) {
        let mut over;
        (self[idx], over) = self[idx].overflowing_sub(val);
        while over {
            idx += 1;
            if idx == self.len() {panic!("attempt to subtract with overflow")}
            (self[idx], over) = self[idx].overflowing_sub(1);
        }
    }

    fn indexed_mul(&self, idx: usize, val: u64) -> Self {
        let mut temp = Vln::with_digits(self.digits());

        for i in 0..self.len() {
            let a1 = self[i] >> 32; //a1 < 2^32
            let a0 = self[i] & 0xffffffff; //a2 < 2^32
            let b1 = val >> 32; //b1 < 2^32
            let b0 = val & 0xffffffff; //b2 < 2^32
            let a1b1 = a1 * b1;
            let a0b0 = a0 * b0;
            let a0b1 = a0 * b1;
            let b0a1 = b0 * a1;
            //* is there a way to reduce adds? */
            temp.indexed_add(i + idx + 1, a1b1);
            temp.indexed_add(i + idx + 1, a0b1 >> 32);
            temp.indexed_add(i + idx + 1, b0a1 >> 32);
            temp.indexed_add(i + idx, a0b1 << 32);
            temp.indexed_add(i + idx, b0a1 << 32);
            // println!("{:?}, {:?}, {:?}, {:?}, {:?}", self[i], val, i, i + idx, a0b0);
            temp.indexed_add(i + idx, a0b0);
        }

        temp
    }

    fn mul_64(lhs: u64, rhs: u64) -> (u64, u64) {
        let a1 = lhs >> 32; //a1 < 2^32
        let a0 = lhs & 0xffffffff; //a2 < 2^32
        let b1 = rhs >> 32; //b1 < 2^32
        let b0 = rhs & 0xffffffff; //b2 < 2^32
        let a1b1 = a1 * b1;
        let a0b0 = a0 * b0;
        let a0b1_b0a1 = (a0 + b1) * (b0 + a1) - a1b1 - a0b0;
        (a1b1 + a0b1_b0a1 >> 32, a0b0 + a0b1_b0a1 << 32)
    }

    // a = [1, 3, 4, 1]
    // b = [3, 1, 1, 2]
    //TODO lots of new Vln's are created, implicitly and explicitly, probably are inneficant
    //*With Very-very-very large numbers we may hit a recursion limit
    fn mul_karatsuba(a: &Vln, b: &Vln) -> Vln {
        //base case
        if a.len() <= 1 && b.len() <= 1 {
            let result = Vln::mul_64(a[0], b[0]);
            return Vln::new(&[result.0, result.1]);
        }

        // split numbers roughly in half
        let (rhs, lhs) = if a.len() > b.len() {(a, b)} else {(b, a)};
        let mid = rhs.len() / 2;
        let a1 = Vln::new(&rhs[mid..]);
        let a0 = Vln::new(&rhs[..mid]);
        let (b0, b1) =
            if mid <= lhs.len() {(Vln::new(&lhs[..mid]), Vln::new(&lhs[mid..]))}
            else {(Vln::new(&lhs[..]), Vln{value: Vec::new()})};


        let ll = Vln::mul_karatsuba(&a0, &b0);
        let mut hh = Vln::mul_karatsuba(&a1, &b1);
        let mut lh = &Vln::mul_karatsuba(&(a1 + a0), &(b1 + b0)) - &(&ll + &hh);

        hh <<= mid * 2;
        lh <<= mid;

        hh + lh + ll
    }
}

//TODO test which Add is fastest, a + b, &a + &b, a += &b
//TODO is it worth having &a + b, a + &b, or any impls for mutable references
impl Add for &Vln {
    type Output=Vln;

    fn add(self, rhs: Self) -> Self::Output {
        let mut temp = self.clone();
        for (lval_idx, rval) in (0..self.len()).zip(rhs.iter()) {
            temp.indexed_add(lval_idx, *rval);
        }
        temp
    }
}

impl Add for Vln {
    type Output=Vln;

    fn add(mut self, rhs: Self) -> Self::Output {
        for (lval_idx, rval) in (0..self.len()).zip(rhs.iter()) {
            self.indexed_add(lval_idx, *rval);
        }
        self
    }
}

impl AddAssign<&Self> for Vln {
    fn add_assign(&mut self, rhs: &Self) {
        for (lval_idx, rval) in (0..self.len()).zip(rhs.iter()) {
            self.indexed_add(lval_idx, *rval);
        }
    }
}

impl Sub for &Vln {
    type Output=Vln;

    fn sub(self, rhs: Self) -> Self::Output {
        let mut temp = self.clone();
        for (lval_idx, rval) in (0..self.len()).zip(rhs.iter()) {
            temp.indexed_sub(lval_idx, *rval);
        }
        temp
    }
}

impl Sub for Vln {
    type Output=Vln;

    fn sub(mut self, rhs: Self) -> Self::Output {
        for (lval_idx, rval) in (0..self.len()).zip(rhs.iter()) {
            self.indexed_sub(lval_idx, *rval);
        }
        self
    }
}

impl SubAssign<&Self> for Vln {
    fn sub_assign(&mut self, rhs: &Self) {
        for (lval_idx, rval) in (0..self.len()).zip(rhs.iter()) {
            self.indexed_sub(lval_idx, *rval);
        }
    }
}

impl<T: Into<u64>> Add<T> for Vln {
    type Output = Vln;

    fn add(mut self, rhs: T) -> Self::Output {
        self.indexed_add(0, rhs.into());
        self
    }
}

impl<T: Into<u64>> AddAssign<T> for Vln {
    fn add_assign(&mut self, rhs: T) {
        self.indexed_add(0, rhs.into());
    }
}

impl Mul for &Vln {
    type Output = Vln;

    fn mul(self, rhs: Self) -> Self::Output {
        let mut accum = Vln::with_digits(self.digits() + rhs.digits());
        for (rval_idx, rval) in rhs.iter().enumerate() {
            accum += &self.indexed_mul(rval_idx, *rval);
        }
        accum
    }
}

impl MulAssign<&Self> for Vln {
    fn mul_assign(&mut self, rhs: &Self) {
        *self = self as &Vln * rhs;
    }
}

impl<T: Into<u64>> Mul<T> for Vln {
    type Output = Vln;

    fn mul(self, rhs: T) -> Self::Output {
        &self + &self.indexed_mul(0, rhs.into())
    }
}

impl<T: Into<u64>> MulAssign<T> for Vln {
    fn mul_assign(&mut self, rhs: T) {
        *self += &self.indexed_mul(0, rhs.into());
    }
}

impl PartialEq for Vln {
    fn eq(&self, other: &Self) -> bool {
        for (a, b) in self.iter().zip(other.iter()) {
            if a != b {return false;}
        }
        true
    }
}

impl<T: TryInto<usize>> ShlAssign<T> for Vln {
    fn shl_assign(&mut self, rhs: T) {
        let rhs_size = rhs.try_into().ok().unwrap();
        if self.value.capacity() < self.value.len() + rhs_size {
            self.value.resize(self.len() + rhs_size, 0);
        }
        self.value.copy_within(0..rhs_size, rhs_size);
    }
}

impl<T: TryInto<usize>> ShrAssign<T> for Vln {
    fn shr_assign(&mut self, rhs: T) {
        self.value.copy_within(TryInto::<usize>::try_into(rhs).ok().unwrap().., 0);
    }
}

impl Eq for Vln{}

impl Deref for Vln {
    type Target = [u64];
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl DerefMut for Vln {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl<T: Into<u64> + Clone> From<&[T]> for Vln {
    fn from(vals: &[T]) -> Self {
        Vln{value: vals.iter().map(|val| (*val).clone().into()).collect()}
    }
}

#[cfg(test)]
mod tests {
    use crate::Vln;

    #[test]
    fn test_add() {
        let advancer = u64::MAX / 2;
        let mut vln = Vln::new(&[0u8]);
        vln = vln + advancer;
        assert!(vln[0] == u64::MAX / 2);
        vln = vln + advancer;
        assert!(vln[0] == u64::MAX - 1);
        vln  = vln + advancer;
        assert!(vln[1] == 1);
        assert!(vln[0] == u64::MAX / 2 - 2);

        let advancer = Vln::new(&[u64::MAX / 2]);
        let mut vln = Vln::new(&[0u8]);
        vln = &vln + &advancer;
        assert!(vln[0] == u64::MAX / 2);
        vln = &vln + &advancer;
        assert!(vln[0] == u64::MAX - 1);
        vln = &vln + &advancer;
        assert!(vln[1] == 1);
        assert!(vln[0] == u64::MAX / 2 - 2);

        let advancer = Vln::new(&[u64::MAX / 2]);
        let mut vln = Vln::new(&[0u8]);
        vln += &advancer;
        assert!(vln[0] == u64::MAX / 2);
        vln += &advancer;
        assert!(vln[0] == u64::MAX - 1);
        vln += &advancer;
        assert!(vln[1] == 1);
        assert!(vln[0] == u64::MAX / 2 - 2);
    }

    #[test]
    fn test_add_eq() {
        let a = Vln::new(&[u64::MAX, u64::MAX, u64::MAX]);
        let b = Vln::new(&[0u8, 0u8, 0u8, 1u8]);
        assert_eq!(b, a + 1u8);

        let a = Vln::new(&[u64::MAX, u64::MAX, u64::MAX, 0xf]);
        let b = Vln::new(&[0u8, 0u8, 0u8, 0x10u8]);
        assert_eq!(b, a + 1u8);
    }

    #[test]
    fn test_sub() {
        let a = Vln::from(&[134817u32][..]);
        let b = Vln::from(&[13414u32][..]);
        assert_eq!(a - b, Vln::new(&[121403u32]));

        let a = Vln::from(&[2896410004u32, 1u32][..]);
        let b = Vln::from(&[3905672818u32][..]);
        assert_eq!(a - b, Vln::new(&[18446744072700288802u64]));

        let a = Vln::from(&[0, 2896410004u32, 1u32][..]);
        let b = Vln::from(&[0, 3905672818u32][..]);
        assert_eq!(a - b, Vln::new(&[0, 18446744072700288802u64]));

        let a = Vln::from(&[2896410004u32, 0, 1u32][..]);
        let b = Vln::from(&[3905672818u32][..]);
        assert_eq!(a - b, Vln::new(&[18446744072700288802u64, u64::MAX, 0]));
    }

    #[test]
    fn test_mul() {
        let a = Vln::new(&[0u64, 1u64]);
        let b = Vln::new(&[0u64, 0u64, 1u64]);
        assert_eq!(&a * &a, b);

        let a = Vln::new(&[0u64, 1u64]);
        let b = Vln::new(&[1u64, 0u64, 1u64]);
        let c = Vln::new(&[0u64, 1u64, 0u64, 1u64]);
        assert_eq!(&a * &b, c);

        let a = Vln::new(&[0xffffffffffffu64, 0u64]);
        let b = Vln::new(&[0xeeeeeeeeeeeeu64, 0u64, 1u64]);
        let c = Vln::new(&[0xEEED111111111112u64, 0xEEEEEEEEu64, 0xffffffffffffu64, 0u64, 0u64]);
        assert_eq!(&a * &b, c);
    }
}
