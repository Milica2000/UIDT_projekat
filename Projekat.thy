theory Projekat
  imports Main "HOL-Library.Char_ord"

begin

(* Funkcije za sortiranje liste listi (liste reci) *)

(* Leksikografsko poredjenje dve reci(liste) *)
fun leks :: "'a::linorder list \<Rightarrow> 'a list \<Rightarrow> bool" where
  "leks [] [] = True" |
  "leks [] _  = True" |
  "leks _ []  = False" |
  "leks (x#xs) (y#ys) = (if x < y then True else if x = y then leks xs ys else False)"


(* Ova funkcija ubacuje rec u listu sortiranih reci *)
fun ubaci :: "'a::linorder list \<Rightarrow> 'a list list \<Rightarrow> 'a list list" where
  "ubaci x [] = [x]" |
  "ubaci x (y # ys) = (if leks x y then x # y # ys else y # ubaci x ys)"

(* Sortiramo listu listi leksikografski*)
fun sort1 :: "'a::linorder list list \<Rightarrow> 'a list list" where
  "sort1 [] = []" |
  "sort1 (x # xs) = ubaci x (sort1 xs)"




(*funkcija za racunanje pozicije polazne reci u matrici sortiranih svih rotacija reci*)
fun position :: "'a list \<Rightarrow> 'a list list \<Rightarrow> nat" where
  "position xs xss =
 length (takeWhile (\<lambda>rec_u_matrici. rec_u_matrici \<noteq> xs) xss)"

(*rotacija jedne reci*)
fun lrot :: "'a list \<Rightarrow> 'a list" where
  "lrot [] = []"
| "lrot (x#xs) = xs @ [x]"

(*ova funkacija broji koliko puta dodajemo rotiranu prethodnu rec u skup svih rotacija
reci*)
fun broj_rotacije :: "'a list \<Rightarrow> nat \<Rightarrow> 'a list list" where
  "broj_rotacije xs 0 = []"
| "broj_rotacije xs (Suc n) = xs # broj_rotacije (lrot xs) n"

(*ova funkacija vraca listu svih rotacija jedne reci*)
fun rotacije :: "'a list \<Rightarrow> 'a list list" where
  "rotacije xs = broj_rotacije xs (length xs)"


(* Funkcija koja vraca poslednja slova iz matrice sortiranih rotacija *)
fun poslednja_slova :: "'a list list \<Rightarrow> 'a list" where
  "poslednja_slova [] = []"
| "poslednja_slova (x#xs) = last x # poslednja_slova xs"



(* BWT  *)
fun transform :: "'a::linorder list \<Rightarrow> ('a list * nat)" where
  "transform xs = 
     (let xss = sort1 (rotacije xs)
      in (poslednja_slova xss, position xs xss))"

value "transform ''yokohama''"         
value "transform ''banana''"       
value "transform ''abc''" 

end