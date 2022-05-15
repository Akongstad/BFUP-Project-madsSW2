module internal ActionGenerator
open MultiSet
open ScrabbleUtil.Dictionary
open madsSW2.State
open ScrabbleUtil

val generateAction: state -> (coord * (uint32 * (char * int))) list//(coo

