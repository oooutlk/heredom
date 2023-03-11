//! Tuple notations to describe XML-like trees with the ability to traverse in preorder/postorder.

use tuplex::*;

/// A struct keeping the node's children count and all its descendants count.
#[derive(Copy,Clone,Debug,Default,PartialEq,Eq)]
pub struct Size {
    pub degree      : usize, // count of child nodes
    pub descendants : usize, // count of its descendant nodes
}

/// Visit one node in tree/forest building process, using tuple notations.
#[derive( Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash )]
pub enum Visit<T,A> {
    Branch( T ),
    Leaf(   T ),
    Frame,
    AttrsStart( usize ),
    Attr( A ),
    AttrsEnd,
}

/// For tuple notations to construct a dom tree.
pub trait DomTree<T,A,Shape>: Sized {
    const SIZE: Size;
    fn descendants( indirect_level: usize ) -> usize;
    fn height() -> usize;
    fn preorder(                 self, f: &mut impl FnMut( Visit<T,A>       ));
    fn preorder_with_size_hint(  self, f: &mut impl FnMut( Visit<T,A>, Size ));
    fn postorder(                self, f: &mut impl FnMut( Visit<T,A>       ));
    fn postorder_with_size_hint( self, f: &mut impl FnMut( Visit<T,A>, Size ));

    fn try_preorder<Error>(                 self, f: &mut impl FnMut( Visit<T,A>       ) -> Result<(),Error> ) -> Result<(),Error>;
    fn try_postorder<Error>(                self, f: &mut impl FnMut( Visit<T,A>       ) -> Result<(),Error> ) -> Result<(),Error>;
    fn try_preorder_with_size_hint<Error>(  self, f: &mut impl FnMut( Visit<T,A>, Size ) -> Result<(),Error> ) -> Result<(),Error>;
    fn try_postorder_with_size_hint<Error>( self, f: &mut impl FnMut( Visit<T,A>, Size ) -> Result<(),Error> ) -> Result<(),Error>;
}

/// Node with tag and attributes.
pub struct Node<Tag,Attrs>( pub Tag, pub Attrs );

fn enumerate_attrs<T,A>( attrs: impl IntoHomoTuple<A>, f: &mut impl FnMut( Visit<T,A> )) {
    let tuple = attrs.into_homo_tuple();
    let len = tuple.len();
    f( Visit::AttrsStart( len ));
    let iter = tuple.wrap_into_iter();
    iter.for_each( |attr| f( Visit::Attr( attr )));
    f( Visit::AttrsEnd );
}

fn enumerate_attrs_with_size_hint<T,A>( attrs: impl IntoHomoTuple<A>, f: &mut impl FnMut( Visit<T,A>, Size )) {
    let tuple = attrs.into_homo_tuple();
    let len = tuple.len();
    f( Visit::AttrsStart( len ), Size::default() );
    let iter = tuple.wrap_into_iter();
    iter.for_each( |attr| f( Visit::Attr( attr ), Size::default() ));
    f( Visit::AttrsEnd, Size::default() );
}

fn try_enumerate_attrs<T,A,Error>( attrs: impl IntoHomoTuple<A>, f: &mut impl FnMut( Visit<T,A> ) -> Result<(),Error> ) -> Result<(),Error> {
    let tuple = attrs.into_homo_tuple();
    let len = tuple.len();
    f( Visit::AttrsStart( len ))?;
    let mut iter = tuple.wrap_into_iter();
    iter.try_for_each( |attr| f( Visit::Attr( attr )))?;
    f( Visit::AttrsEnd )
}

fn try_enumerate_attrs_with_size_hint<T,A,Error>( attrs: impl IntoHomoTuple<A>, f: &mut impl FnMut( Visit<T,A>, Size ) -> Result<(),Error> ) -> Result<(),Error> {
    let tuple = attrs.into_homo_tuple();
    let len = tuple.len();
    f( Visit::AttrsStart( len ), Size::default() )?;
    let mut iter = tuple.wrap_into_iter();
    iter.try_for_each( |attr| f( Visit::Attr( attr ), Size::default() ))?;
    f( Visit::AttrsEnd, Size::default() )
}

impl<T,A,Attrs> DomTree<T,A,Attrs> for Node<T,Attrs>
    where Attrs: IntoHomoTuple<A>
{
    const SIZE: Size = Size{ degree: 0, descendants: 0 };
    fn descendants( _indirect_level: usize ) -> usize { 0 }
    fn height() -> usize { 1 }

    fn preorder( self, f: &mut impl FnMut( Visit<T,A> ) ) {
        f( Visit::Leaf( self.0 ));
        enumerate_attrs( self.1, f );
    }

    fn preorder_with_size_hint( self, f: &mut impl FnMut( Visit<T,A>, Size )) {
        f( Visit::Leaf( self.0 ), Size::default() );
        enumerate_attrs_with_size_hint( self.1, f );
    }

    fn postorder( self, f: &mut impl FnMut( Visit<T,A> ) ) {
        f( Visit::Leaf( self.0 ));
        enumerate_attrs( self.1, f );
    }

    fn postorder_with_size_hint( self, f: &mut impl FnMut( Visit<T,A>, Size )) {
        f( Visit::Leaf( self.0 ), Size::default() );
        enumerate_attrs_with_size_hint( self.1, f );
    }

    fn try_preorder<Error>( self, f: &mut impl FnMut( Visit<T,A> ) -> Result<(),Error> ) -> Result<(),Error> {
        f( Visit::Leaf( self.0 ))?;
        try_enumerate_attrs( self.1, f )
    }

    fn try_postorder<Error>( self, f: &mut impl FnMut( Visit<T,A> ) -> Result<(),Error> ) -> Result<(),Error> {
        f( Visit::Leaf( self.0 ))?;
        try_enumerate_attrs( self.1, f )
    }

    fn try_preorder_with_size_hint<Error>(  self, f: &mut impl FnMut( Visit<T,A>, Size ) -> Result<(),Error> ) -> Result<(),Error> {
        f( Visit::Leaf( self.0 ), Size::default() )?;
        try_enumerate_attrs_with_size_hint( self.1, f )
    }

    fn try_postorder_with_size_hint<Error>( self, f: &mut impl FnMut( Visit<T,A>, Size ) -> Result<(),Error> ) -> Result<(),Error> {
        f( Visit::Leaf( self.0 ), Size::default() )?;
        try_enumerate_attrs_with_size_hint( self.1, f )
    }
}

macro_rules! impl_tuple_tree {
    ($len:expr => ($($n:tt $name:ident $shape:ident)*)) => {
        impl<T,A,Attrs$(,$name,$shape)*> DomTree<T,A,(Attrs,$($shape),*)> for (Node<T,Attrs>,$($name,)*)
            where Node<T,Attrs>: DomTree<T,A,Attrs> $(,$name: DomTree<T,A,$shape>)*
                , Attrs: IntoHomoTuple<A>
        {
            const SIZE: Size = Size {
                degree     : $len,
                descendants: 0 $(+ <$name as DomTree<T,A,$shape>>::SIZE.descendants+1 )*,
            };

            fn descendants( indirect_level: usize ) -> usize {
                if indirect_level == 0 {
                    $len
                } else {
                    0 $( + <$name as DomTree<T,A,$shape>>::descendants( indirect_level-1 ) )*
                }
            }

            fn height() -> usize {
                1 + *[ 0 $(, <$name as DomTree<T,A,$shape>>::height() )* ].iter().max_by( |x,y| x.cmp(y) ).unwrap()
            }

            fn preorder( self, f: &mut impl FnMut( Visit<T,A> ) ) {
                let Node( name, attrs ) = self.0;
                if <Self as DomTree<T,A,(Attrs,$($shape),*)>>::SIZE.degree == 0 {
                    f( Visit::Leaf( name ));
                    enumerate_attrs( attrs, f );
                } else {
                    f( Visit::Branch( name ));
                    enumerate_attrs( attrs, f );
                    $( (self.$n).preorder( f ); )*
                    f( Visit::Frame );
                }
            }

            fn preorder_with_size_hint( self, f: &mut impl FnMut( Visit<T,A>, Size )) {
                let Node( name, attrs ) = self.0;
                let size = <Self as DomTree<T,A,(Attrs,$($shape),*)>>::SIZE;
                if size.degree == 0 {
                    f( Visit::Leaf( name ), size );
                    enumerate_attrs_with_size_hint( attrs, f );
                } else {
                    f( Visit::Branch( name ), size );
                    enumerate_attrs_with_size_hint( attrs, f );
                    $( (self.$n).preorder_with_size_hint( f ); )*
                    f( Visit::Frame, size );
                }
            }

            fn postorder( self, f: &mut impl FnMut( Visit<T,A> ) ) {
                let Node( name, attrs ) = self.0;
                if <Self as DomTree<T,A,(Attrs,$($shape),*)>>::SIZE.degree == 0 {
                    f( Visit::Leaf( name ));
                    enumerate_attrs( attrs, f );
                } else {
                    f( Visit::Frame );
                    $( (self.$n).postorder( f ); )*
                    f( Visit::Branch( name ));
                    enumerate_attrs( attrs, f );
                }
            }

            fn postorder_with_size_hint( self, f: &mut impl FnMut( Visit<T,A>, Size )) {
                let Node( name, attrs ) = self.0;
                let size = <Self as DomTree<T,A,(Attrs,$($shape),*)>>::SIZE;
                if size.degree == 0 {
                    f( Visit::Leaf( name ), size );
                    enumerate_attrs_with_size_hint( attrs, f );
                } else {
                    f( Visit::Frame, size );
                    $( (self.$n).postorder_with_size_hint( f ); )*
                    f( Visit::Branch( name ), size );
                    enumerate_attrs_with_size_hint( attrs, f );
                }
            }

            fn try_preorder<Error>( self, f: &mut impl FnMut( Visit<T,A> ) -> Result<(),Error> ) -> Result<(),Error> {
                let Node( name, attrs ) = self.0;
                if <Self as DomTree<T,A,(Attrs,$($shape),*)>>::SIZE.degree == 0 {
                    f( Visit::Leaf( name ))?;
                    try_enumerate_attrs( attrs, f )
                } else {
                    f( Visit::Branch( name ))?;
                    try_enumerate_attrs( attrs, f )?;
                    $( (self.$n).try_preorder( f )?; )*
                    f( Visit::Frame )
                }
            }

            fn try_postorder<Error>( self, f: &mut impl FnMut( Visit<T,A> ) -> Result<(),Error> ) -> Result<(),Error> {
                let Node( name, attrs ) = self.0;
                if <Self as DomTree<T,A,(Attrs,$($shape),*)>>::SIZE.degree == 0 {
                    f( Visit::Leaf( name ))?;
                    try_enumerate_attrs( attrs, f )
                } else {
                    f( Visit::Frame )?;
                    $( (self.$n).try_postorder( f )?; )*
                    f( Visit::Branch( name ))?;
                    try_enumerate_attrs( attrs, f )
                }

            }

            fn try_preorder_with_size_hint<Error>(  self, f: &mut impl FnMut( Visit<T,A>, Size ) -> Result<(),Error> ) -> Result<(),Error> {
                let Node( name, attrs ) = self.0;
                let size = <Self as DomTree<T,A,(Attrs,$($shape),*)>>::SIZE;
                if size.degree == 0 {
                    f( Visit::Leaf( name ), size )?;
                    try_enumerate_attrs_with_size_hint( attrs, f )
                } else {
                    f( Visit::Branch( name ), size )?;
                    try_enumerate_attrs_with_size_hint( attrs, f )?;
                    $( (self.$n).try_preorder_with_size_hint( f )?; )*
                    f( Visit::Frame, size )
                }
            }

            fn try_postorder_with_size_hint<Error>( self, f: &mut impl FnMut( Visit<T,A>, Size ) -> Result<(),Error> ) -> Result<(),Error> {
                let Node( name, attrs ) = self.0;
                let size = <Self as DomTree<T,A,(Attrs,$($shape),*)>>::SIZE;
                if size.degree == 0 {
                    f( Visit::Leaf( name ), size )?;
                    try_enumerate_attrs_with_size_hint( attrs, f )
                } else {
                    f( Visit::Frame, size )?;
                    $( (self.$n).try_postorder_with_size_hint( f )?; )*
                    f( Visit::Branch( name ), size )?;
                    try_enumerate_attrs_with_size_hint( attrs, f )
                }
            }
        }
    }
}

/// For tuple notations to construct a dom forest.
pub trait DomForest<T,A,Shape>: Sized {
    const SIZE: Size;
    fn descendants( indirect_level: usize ) -> usize;
    fn height() -> usize;

    fn preorder(                            self, f: &mut impl FnMut( Visit<T,A>       ));
    fn postorder(                           self, f: &mut impl FnMut( Visit<T,A>       ));
    fn preorder_with_size_hint(             self, f: &mut impl FnMut( Visit<T,A>, Size ));
    fn postorder_with_size_hint(            self, f: &mut impl FnMut( Visit<T,A>, Size ));

    fn try_preorder<Error>(                 self, f: &mut impl FnMut( Visit<T,A>       ) -> Result<(),Error> ) -> Result<(),Error>;
    fn try_postorder<Error>(                self, f: &mut impl FnMut( Visit<T,A>       ) -> Result<(),Error> ) -> Result<(),Error>;
    fn try_preorder_with_size_hint<Error>(  self, f: &mut impl FnMut( Visit<T,A>, Size ) -> Result<(),Error> ) -> Result<(),Error>;
    fn try_postorder_with_size_hint<Error>( self, f: &mut impl FnMut( Visit<T,A>, Size ) -> Result<(),Error> ) -> Result<(),Error>;
}

macro_rules! impl_tuple_forest {
    ($len:expr => ($($n:tt $name:ident $shape:ident)*)) => {
        impl<$($name,$shape,)*T,A> DomForest<T,A,((),$($shape,)*)> for ($($name,)*)
            where Node<T,()>: DomTree<T,A,()> $(,$name: DomTree<T,A,$shape>)*
        {
            const SIZE: Size = Size {
                degree     : $len,
                descendants: 0 $(+ <$name as DomTree<T,A,$shape>>::SIZE.descendants+1 )*,
            };

            fn descendants( indirect_level: usize ) -> usize {
                if indirect_level == 0 {
                    $len
                } else {
                    0 $( + <$name as DomTree<T,A,$shape>>::descendants( indirect_level-1 ) )*
                }
            }

            fn height() -> usize {
                0 + *[ 0 $(, <$name as DomTree<T,A,$shape>>::height() )* ].iter().max_by( |x,y| x.cmp(y) ).unwrap()
            }

            fn preorder( self, _f: &mut impl FnMut( Visit<T,A> ) ) {
                $( (self.$n).preorder( _f ); )*
            }

            fn preorder_with_size_hint(  self, _f: &mut impl FnMut( Visit<T,A>, Size )) {
                $( (self.$n).preorder_with_size_hint( _f ); )*
            }

            fn postorder( self, _f: &mut impl FnMut( Visit<T,A> ) ) {
                $( (self.$n).postorder( _f ); )*
            }

            fn postorder_with_size_hint(  self, _f: &mut impl FnMut( Visit<T,A>, Size )) {
                $( (self.$n).postorder_with_size_hint( _f ); )*
            }

            fn try_preorder<Error>( self, _f: &mut impl FnMut( Visit<T,A> ) -> Result<(),Error> ) -> Result<(),Error> {
                $( (self.$n).try_preorder( _f )?; )*
                Ok(())
            }

            fn try_postorder<Error>( self, _f: &mut impl FnMut( Visit<T,A> ) -> Result<(),Error> ) -> Result<(),Error> {
                $( (self.$n).try_postorder( _f )?; )*
                Ok(())
            }

            fn try_preorder_with_size_hint<Error>(  self, _f: &mut impl FnMut( Visit<T,A>, Size ) -> Result<(),Error> ) -> Result<(),Error> {
                $( (self.$n).try_preorder_with_size_hint( _f )?; )*
                Ok(())
            }

            fn try_postorder_with_size_hint<Error>( self, _f: &mut impl FnMut( Visit<T,A>, Size ) -> Result<(),Error> ) -> Result<(),Error> {
                $( (self.$n).try_postorder_with_size_hint( _f )?; )*
                Ok(())
            }
        }
    }
}

macro_rules! tup_tr_impls {
    ($($len:expr => ($($n:tt $name:ident $shape:ident)*))+) => {$(
        impl_tuple_tree!( $len => ($($n $name $shape)*) );
    )+};
}

tup_tr_impls! {
    0 => ()
    1 => (1 T1 S1)
    2 => (1 T1 S1 2 T2 S2)
    3 => (1 T1 S1 2 T2 S2 3 T3 S3)
    4 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4)
    5 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5)
    6 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6)
    7 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7)
    8 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8)
    9 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9)
   10 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10)
   11 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11)
   12 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12)
   13 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13)
   14 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14)
   15 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15)
   16 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16)
   17 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17)
   18 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18)
   19 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19)
   20 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20)
   21 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20 21 T21 S21)
   22 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20 21 T21 S21 22 T22 S22)
   23 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20 21 T21 S21 22 T22 S22 23 T23 S23)
   24 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20 21 T21 S21 22 T22 S22 23 T23 S23 24 T24 S24)
   25 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20 21 T21 S21 22 T22 S22 23 T23 S23 24 T24 S24 25 T25 S25)
   26 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20 21 T21 S21 22 T22 S22 23 T23 S23 24 T24 S24 25 T25 S25 26 T26 S26)
   27 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20 21 T21 S21 22 T22 S22 23 T23 S23 24 T24 S24 25 T25 S25 26 T26 S26 27 T27 S27)
   28 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20 21 T21 S21 22 T22 S22 23 T23 S23 24 T24 S24 25 T25 S25 26 T26 S26 27 T27 S27 28 T28 S28)
   29 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20 21 T21 S21 22 T22 S22 23 T23 S23 24 T24 S24 25 T25 S25 26 T26 S26 27 T27 S27 28 T28 S28 29 T29 S29)
   30 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20 21 T21 S21 22 T22 S22 23 T23 S23 24 T24 S24 25 T25 S25 26 T26 S26 27 T27 S27 28 T28 S28 29 T29 S29 30 T30 S30)
   31 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20 21 T21 S21 22 T22 S22 23 T23 S23 24 T24 S24 25 T25 S25 26 T26 S26 27 T27 S27 28 T28 S28 29 T29 S29 30 T30 S30 31 T31 S31)
   32 => (1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20 21 T21 S21 22 T22 S22 23 T23 S23 24 T24 S24 25 T25 S25 26 T26 S26 27 T27 S27 28 T28 S28 29 T29 S29 30 T30 S30 31 T31 S31 32 T32 S32)
}

macro_rules! tup_fr_impls {
    ($($len:expr => ($($n:tt $name:ident $shape:ident)*))+) => {$(
        impl_tuple_forest!( $len => ($($n $name $shape)*) );
    )+};
}

tup_fr_impls! {
    0 => ()
    1 => (0 T0 S0)
    2 => (0 T0 S0 1 T1 S1)
    3 => (0 T0 S0 1 T1 S1 2 T2 S2)
    4 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3)
    5 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4)
    6 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5)
    7 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6)
    8 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7)
    9 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8)
    10 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9)
   11 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10)
   12 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11)
   13 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12)
   14 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13)
   15 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14)
   16 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15)
   17 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16)
   18 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17)
   19 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18)
   20 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19)
   21 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20)
   22 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20 21 T21 S21)
   23 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20 21 T21 S21 22 T22 S22)
   24 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20 21 T21 S21 22 T22 S22 23 T23 S23)
   25 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20 21 T21 S21 22 T22 S22 23 T23 S23 24 T24 S24)
   26 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20 21 T21 S21 22 T22 S22 23 T23 S23 24 T24 S24 25 T25 S25)
   27 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20 21 T21 S21 22 T22 S22 23 T23 S23 24 T24 S24 25 T25 S25 26 T26 S26)
   28 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20 21 T21 S21 22 T22 S22 23 T23 S23 24 T24 S24 25 T25 S25 26 T26 S26 27 T27 S27)
   29 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20 21 T21 S21 22 T22 S22 23 T23 S23 24 T24 S24 25 T25 S25 26 T26 S26 27 T27 S27 28 T28 S28)
   30 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20 21 T21 S21 22 T22 S22 23 T23 S23 24 T24 S24 25 T25 S25 26 T26 S26 27 T27 S27 28 T28 S28 29 T29 S29)
   31 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20 21 T21 S21 22 T22 S22 23 T23 S23 24 T24 S24 25 T25 S25 26 T26 S26 27 T27 S27 28 T28 S28 29 T29 S29 30 T30 S30)
   32 => (0 T0 S0 1 T1 S1 2 T2 S2 3 T3 S3 4 T4 S4 5 T5 S5 6 T6 S6 7 T7 S7 8 T8 S8 9 T9 S9 10 T10 S10 11 T11 S11 12 T12 S12 13 T13 S13 14 T14 S14 15 T15 S15 16 T16 S16 17 T17 S17 18 T18 S18 19 T19 S19 20 T20 S20 21 T21 S21 22 T22 S22 23 T23 S23 24 T24 S24 25 T25 S25 26 T26 S26 27 T27 S27 28 T28 S28 29 T29 S29 30 T30 S30 31 T31 S31)
}

#[cfg( test )]
mod tests {
    use super::*;

    #[test]
    fn tree_preorder() {
        let mut visits = Vec::new();
        let tree = ( Node(0,()), ( Node(1,(11,)), Node(3,(31,32,33)), Node(4,(41,42,43,44)) ), (Node(2,(21,22)), Node(5,(51,52,53,54,55)), Node(6,(61,62,63,64,65,66)) ), );
        DomTree::<i32,i32,_>::preorder( tree, &mut |visit| visits.push( visit ));
        assert_eq!( visits, vec![
            Visit::Branch(0),
            Visit::AttrsStart(0), Visit::AttrsEnd,
            Visit::Branch(1),
            Visit::AttrsStart(1), Visit::Attr(11), Visit::AttrsEnd,
            Visit::Leaf(3),
            Visit::AttrsStart(3), Visit::Attr(31), Visit::Attr(32), Visit::Attr(33), Visit::AttrsEnd,
            Visit::Leaf(4),
            Visit::AttrsStart(4), Visit::Attr(41), Visit::Attr(42), Visit::Attr(43), Visit::Attr(44), Visit::AttrsEnd,
            Visit::Frame,
            Visit::Branch(2),
            Visit::AttrsStart(2), Visit::Attr(21), Visit::Attr(22), Visit::AttrsEnd,
            Visit::Leaf(5),
            Visit::AttrsStart(5), Visit::Attr(51), Visit::Attr(52), Visit::Attr(53), Visit::Attr(54), Visit::Attr(55), Visit::AttrsEnd,
            Visit::Leaf(6),
            Visit::AttrsStart(6), Visit::Attr(61), Visit::Attr(62), Visit::Attr(63), Visit::Attr(64), Visit::Attr(65), Visit::Attr(66), Visit::AttrsEnd,
            Visit::Frame,
            Visit::Frame,
        ]);
    }

    #[test]
    fn forest_preorder() {
        let mut visits = Vec::new();
        let forest = ( ( Node(1,(11,)), Node(3,(31,32,33)), Node(4,(41,42,43,44)) ), (Node(2,(21,22)), Node(5,(51,52,53,54,55)), Node(6,(61,62,63,64,65,66)) ), );
        DomForest::<i32,i32,_>::preorder( forest, &mut |visit| visits.push( visit ));
        assert_eq!( visits, vec![
            Visit::Branch(1),
            Visit::AttrsStart(1), Visit::Attr(11), Visit::AttrsEnd,
            Visit::Leaf(3),
            Visit::AttrsStart(3), Visit::Attr(31), Visit::Attr(32), Visit::Attr(33), Visit::AttrsEnd,
            Visit::Leaf(4),
            Visit::AttrsStart(4), Visit::Attr(41), Visit::Attr(42), Visit::Attr(43), Visit::Attr(44), Visit::AttrsEnd,
            Visit::Frame,
            Visit::Branch(2),
            Visit::AttrsStart(2), Visit::Attr(21), Visit::Attr(22), Visit::AttrsEnd,
            Visit::Leaf(5),
            Visit::AttrsStart(5), Visit::Attr(51), Visit::Attr(52), Visit::Attr(53), Visit::Attr(54), Visit::Attr(55), Visit::AttrsEnd,
            Visit::Leaf(6),
            Visit::AttrsStart(6), Visit::Attr(61), Visit::Attr(62), Visit::Attr(63), Visit::Attr(64), Visit::Attr(65), Visit::Attr(66), Visit::AttrsEnd,
            Visit::Frame,
        ]);
    }

    #[test]
    fn tree_postorder() {
        let mut visits = Vec::new();
        let tree = ( Node(0,()), ( Node(1,(11,)), Node(3,(31,32,33)), Node(4,(41,42,43,44)) ), (Node(2,(21,22)), Node(5,(51,52,53,54,55)), Node(6,(61,62,63,64,65,66)) ), );
        DomTree::<i32,i32,_>::postorder( tree, &mut |visit| visits.push( visit ));
        assert_eq!( visits, vec![
            Visit::Frame,
            Visit::Frame,
            Visit::Leaf(3),
            Visit::AttrsStart(3), Visit::Attr(31), Visit::Attr(32), Visit::Attr(33), Visit::AttrsEnd,
            Visit::Leaf(4),
            Visit::AttrsStart(4), Visit::Attr(41), Visit::Attr(42), Visit::Attr(43), Visit::Attr(44), Visit::AttrsEnd,
            Visit::Branch(1),
            Visit::AttrsStart(1), Visit::Attr(11), Visit::AttrsEnd,
            Visit::Frame,
            Visit::Leaf(5),
            Visit::AttrsStart(5), Visit::Attr(51), Visit::Attr(52), Visit::Attr(53), Visit::Attr(54), Visit::Attr(55), Visit::AttrsEnd,
            Visit::Leaf(6),
            Visit::AttrsStart(6), Visit::Attr(61), Visit::Attr(62), Visit::Attr(63), Visit::Attr(64), Visit::Attr(65), Visit::Attr(66), Visit::AttrsEnd,
            Visit::Branch(2),
            Visit::AttrsStart(2), Visit::Attr(21), Visit::Attr(22), Visit::AttrsEnd,
            Visit::Branch(0),
            Visit::AttrsStart(0), Visit::AttrsEnd,
        ]);
    }

    #[test]
    fn forest_postorder() {
        let mut visits = Vec::new();
        let forest = ( ( Node(1,(11,)), Node(3,(31,32,33)), Node(4,(41,42,43,44)) ), (Node(2,(21,22)), Node(5,(51,52,53,54,55)), Node(6,(61,62,63,64,65,66)) ), );
        DomForest::<i32,i32,_>::postorder( forest, &mut |visit| visits.push( visit ));
        assert_eq!( visits, vec![
            Visit::Frame,
            Visit::Leaf(3),
            Visit::AttrsStart(3), Visit::Attr(31), Visit::Attr(32), Visit::Attr(33), Visit::AttrsEnd,
            Visit::Leaf(4),
            Visit::AttrsStart(4), Visit::Attr(41), Visit::Attr(42), Visit::Attr(43), Visit::Attr(44), Visit::AttrsEnd,
            Visit::Branch(1),
            Visit::AttrsStart(1), Visit::Attr(11), Visit::AttrsEnd,
            Visit::Frame,
            Visit::Leaf(5),
            Visit::AttrsStart(5), Visit::Attr(51), Visit::Attr(52), Visit::Attr(53), Visit::Attr(54), Visit::Attr(55), Visit::AttrsEnd,
            Visit::Leaf(6),
            Visit::AttrsStart(6), Visit::Attr(61), Visit::Attr(62), Visit::Attr(63), Visit::Attr(64), Visit::Attr(65), Visit::Attr(66), Visit::AttrsEnd,
            Visit::Branch(2),
            Visit::AttrsStart(2), Visit::Attr(21), Visit::Attr(22), Visit::AttrsEnd,
        ]);
    }
}
