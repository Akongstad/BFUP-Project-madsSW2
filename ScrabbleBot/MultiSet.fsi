// Insert your MultiSet.fsi file here. All modules must be internal

module internal MultiSet

    type internal MultiSet<'a when 'a: comparison>
        //Creates an empty list
        val empty: MultiSet<'a>

        //given a multiset s , returns true if a s is empty, and false otherwise.
        val isEmpty: MultiSet<'a> -> bool

        //given a multiset s , returns the size of s (the sum of the number of occurrences of all elements in s ).
        val size: MultiSet<'a> -> uint32
        //given an element a and a multiset s , returns true if a is an element of s and false otherwise.
        val contains: 'a -> MultiSet<'a> -> bool

        //given an element a and a multiset s , returns how many occurrences of a there are in s .
        val numItems: 'a -> MultiSet<'a> -> uint32

        //given an element a , a number n , and a multiset s , returns the multiset that has a added to s n times.
        val add: 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>

        //given an element a and a multiset s , returns s with a single instance of a added to it.
        val addSingle: 'a -> MultiSet<'a> -> MultiSet<'a>

        //that given an element a , a number n , and a multiset s , returns the multiset with n occurrences
        //of a from s removed (remember that a multiset cannot have fewer than 0 entries of an element).
        //If s contains n or fewer occurrences of a then the result should contain no occurrences of a .
        val remove: 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>

        //that given an element a and a multiset s returns the multiset where a single occurrence of a has been removed from s .
        //If s does not contain a then return s .
        val removeSingle: 'a -> MultiSet<'a> -> MultiSet<'a>

        //given a folding function f , an accumulator acc and a multiset
        //{(a1,x1), ..., (an,xn)} , returns f (... (f acc a1 x1) ...) an xn) .
        val fold: ('a -> 'b -> uint32 -> 'a) -> 'a -> MultiSet<'b> -> 'a
        //that given a folding function f , an accumulator a multiset {(a1,x1), ..., (an,xn)} ,
        //and an accumulator acc , returns f a1 x1 (... (f an xn acc) ...) . The same reasoning applies to foldBack as fold .
        val foldBack: ('a -> uint32 -> 'b -> 'b) -> MultiSet<'a> -> 'b -> 'b
        //val ofList : 'a list -> MultiSet<'a>
        //val toList : MultiSet<'a> -> 'a list
        //val map : ('a -> 'b) -> MultiSet<'a> -> MultiSet<'b>
        //val union : MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>
        //val sum : MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>
        //val subtract : MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>
        //val intersection : MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>