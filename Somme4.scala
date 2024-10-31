sealed trait EtatSomme4
case object S0 extends EtatSomme4
case object S1 extends EtatSomme4
case object S2 extends EtatSomme4
case object S3 extends EtatSomme4
case object S4 extends EtatSomme4
case object SinkSomme4 extends EtatSomme4 // représentation d'un état interdit duquel on ne peut pas sortir

/**
 * Fonction de transition pour l'AFD qui, à parti de 0, donne toutes les séquences de +1 ou +2
 * qui nous donnent la somme de 4
 * 
 * @param etat L'état actuel (somme actuelle)
 * @param symbole Le symbole représentant l'incrément (+1 ou +2)
 * @return Le nouvel état (nouvelle somme) après la transition
 */
def deltaSomme4(etat: EtatSomme4, symbole: Int): EtatSomme4 = (etat, symbole) match
    case (S0, 1) => S1
    case (S0, 2) => S2
    case (S1, 1) => S2
    case (S1, 2) => S3
    case (S2, 1) => S3
    case (S2, 2) => S4
    case (S3, 1) => S4
    case (_, _)  => SinkSomme4

@main def Somme4 =
    val ex = AFD(Set(1, 2), deltaSomme4, S0, Set(S4))

    val mot1: Mot[Int] = List(1,1,1,1)
    println(s"ex.accept($mot1) -> ${ex.accept(mot1)}")
    val mot2: Mot[Int] = List(1,1,1,2)
    println(s"ex.accept($mot2) -> ${ex.accept(mot2)}")
    val mot3: Mot[Int] = List(1,1,1)
    println(s"ex.accept($mot3)   -> ${ex.accept(mot3)}")

    println(s"ex.solve() -> ${ex.solve()}")

    val lazySolution = ex.lazysolve()
    println(s"ex.lazysolve().take(1).toList -> ${lazySolution.take(1).toList}")
    println(s"ex.lazysolve().take(2).toList -> ${lazySolution.take(2).toList}")
    println(s"ex.lazysolve().take(3).toList -> ${lazySolution.take(3).toList}")
    println(s"ex.lazysolve().take(4).toList -> ${lazySolution.take(4).toList}")
    println(s"ex.lazysolve().take(5).toList -> ${lazySolution.take(5).toList}")
    println(s"ex.lazysolve().take(6).toList -> ${lazySolution.take(6).toList}")
