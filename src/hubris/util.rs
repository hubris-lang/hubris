// use std::iter::{Iterator, IntoIterator};
//
// pub trait SeqIteratorExt : Iterator + Sized {
//     fn sequence<U, F, B, E>(self, f: F) -> Sequence<Self, U, F>
//         where U: IntoIterator<Item=Vec<Result<B, E>>>, F: FnMut(Self::Item) -> U;
// }
//
// impl<I: Iterator> SeqIteratorExt for I {
//     fn sequence<U, F, B, E>(self, f: F) -> Sequence<Self, U, F>
//         where U: IntoIterator<Item=Result<B, E>>>, F: FnMut(Self::Item) -> U {
//         Sequence {
//             iter: self,
//             f: f,
//             current_iter: None,
//         }
//     }
// }
//
// pub struct Sequence<I, U: IntoIterator, F, B, E> {
//     iter: I,
//     f: F,
//     current_iter: Option<U::IntoIter>,
// }
//
// impl<I: Iterator, U: IntoIterator, F> Iterator for Sequence<I, U, F>
//     where F: FnMut(I::Item) -> U,
// {
//     type Item = U::Item;
//
//     #[inline]
//     fn next(&mut self) -> Option<U::Item> {
//         match self.current_iter {
//             None => match self.iter.next() {
//                     None => None,
//                     Some(e) => {
//                         self.current_iter = Some((self.f)(e).into_iter());
//                         self.next()
//                     }
//             },
//             Some(i) => i.next()
//         }
//     }
//
//     // #[inline]
//     // fn size_hint(&self) -> (usize, Option<usize>) {
//     //     let (flo, fhi) = self.frontiter.as_ref().map_or((0, Some(0)), |it| it.size_hint());
//     //     let (blo, bhi) = self.backiter.as_ref().map_or((0, Some(0)), |it| it.size_hint());
//     //     let lo = flo.saturating_add(blo);
//     //     match (self.iter.size_hint(), fhi, bhi) {
//     //         ((0, Some(0)), Some(a), Some(b)) => (lo, a.checked_add(b)),
//     //         _ => (lo, None)
//     //     }
//     // }
// }
