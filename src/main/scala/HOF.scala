object HOF {

def map2 [A , B , C]( f: (A , B ) => C , lst1 : List [A], lst2 : List [B]): List [C] = 
	(lst1, lst2) match{ //Match on tuple with lst1 & lst2
		case (Nil, Nil) => Nil
		case (h::t, h1::t1) => f(h, h1) :: map2(f, t, t1)
		case _ => Nil
	}

def zip [A, B](lst1 : List [A], lst2 : List [B]): List [(A, B)] = 
	(lst1, lst2) match{
		case (Nil, Nil) => Nil
		case (h::t, h1::t1) => (h, h1) :: zip(t, t1)
		case _ => Nil
	}

// merge will help flatten
def flatten [A](lst : List[List[A]]): List [A] = 
	lst match { // Pattern match on List[A]
		case Nil => Nil
		case Nil :: t => flatten(t)
		case (h :: iT):: t => h :: flatten(iT :: t) //lhs is type A & rhs is type List[A]
	}

def flatten3 [A](lst : List[List[List[A]]]): List [A] = 
	lst match{ //Pattern match on List[List[A]]
		case Nil => Nil
		case Nil :: t => flatten3(t)
		case ((h :: Nil) :: oT)::t=> h :: flatten3((oT)::t)
		case ((h :: iT) :: oT)::t =>  //lhs is type A, oT is List[A] & t is List[List[A]]
			h :: flatten3(((iT) :: oT) :: t) 
		case _ => List()
	}

val x = 0
def buildList [A](length : Int, f: Int => A) : List [A] =
 	length match{
 		case 0 => Nil
 		case t => f(x)::buildListHelp(t, f, x+1)
 	}

def buildListHelp [A](length : Int, f: Int => A, x: Int) : List[A] =
 	length match{ 
 			case 0 => Nil
 			case t =>{ 
 				if(x<t) f(x)::buildListHelp(t, f, x+1)
 				else Nil
 			}
 	}

def mapList [A, B](alist: List[A], f: A => List[B]): List [B] = 
		alist match {
			case Nil => Nil
			case h :: t => flatten(List(f(h), mapList(t, f)))
		}

def partition [A](f : A => Boolean, alist : List [A]): (List [A], List [A]) =
	alist match{
		case Nil => (Nil,Nil)
		case h::t => { 
			f(h) match{
				case true => (flatten(List(List(h), partition(f, t)._1)), partition(f, t)._2)
				case false => (partition(f, t)._1, flatten(List(List(h), partition(f, t)._2)))
			}
		}	
	}

def merge [A](lessThan : (A, A) => Boolean, alist1 : List [A], alist2 : List [A]): List [A] = {
	(alist1, alist2) match{
		case (Nil, Nil) => Nil
		case (h::t, Nil) => h::t
		case (Nil, h1::t1) => h1::t1
		case (h::t, h1::t1) => {
			if(lessThan(h, h1)) h1::merge(lessThan, h::t, t1) 
			else h::merge(lessThan, t, h1::t1)
		}
	}
}

def sort [A](lessThan : (A, A) => Boolean, alist : List [A]): List [A] =  //in decending order
	alist match{
		case List() => List()
		case h::t => ins(lessThan, t, h)
	}

def ins[A](f: (A, A) => Boolean, lst: List[A], a: A): List[A] = 
	lst match{
		case List() => List(a)
		case h::t => if(f(a,h)) merge(f, List(a), sort(f, h::t)) else merge(f, List(h), sort(f, a::t))
		}
}