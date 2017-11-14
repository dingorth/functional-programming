(*
- napełnić jedną ze szklanek wodą z kranu (FILL),
- opróżnić jedną ze szklanek do zlewu (DRAIN),
- przelać wodę z jednej ze szklanek do innej (TRANSFER). *)

(* para ([4; 9], [4; 6]) oznacza że dysponujemy dwiema szklankami, 
oraz że pierwsza z nich jest pełna, zaś w drugiej znajduje się 6 jednostek wody *)

(* Użyj leniwych list aby zdefiniować funkcję 
nsols : (int list * int) -> int -> move list list, 
taką że nsols (glasses, volume) n zwróci listę n najkrótszych rozwiązań 
problemu zadanego przez glasses i volume (typ danych move powinien reprezentować 
pojedynczy ruch).  *)

type move = Fill of int | Drain of int | Transfer of int * int;;
type solution = move list;;
type state = int list * int list