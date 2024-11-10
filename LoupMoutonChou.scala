sealed trait Intervenant
case object P extends Intervenant
case object L extends Intervenant
case object M extends Intervenant
case object C extends Intervenant

type EtatLMC = (Set[Intervenant], Set[Intervenant])

val etatInitial: EtatLMC = (Set(P, L, M, C), Set())
val etatFinal: EtatLMC = (Set(), Set(P, L, M, C))

/**
 * Fonction pour vérifier si un état est interdit 
 * car le loup dévole le mouton ou le mouton mange le chou
 * 
 * @param etat l'état dont on veut vérifier l'interdiction
 * @return true si l'état est interdit, false sinon
*/
def estInterdit(etat: EtatLMC): Boolean =
    val (gauche, droite) = etat
    (!gauche.contains(P) && gauche.contains(L) && gauche.contains(M)) || // Le loup dévore le mouton à gauche
    (!droite.contains(P) && droite.contains(L) && droite.contains(M)) || // Le loup dévore le mouton à droite
    (!gauche.contains(P) && gauche.contains(M) && gauche.contains(C)) || // Le mouton mange le chou à gauche
    (!droite.contains(P) && droite.contains(M) && droite.contains(C))    // Le mouton mange le chou à droite

/**
 * Fonction de transition pour déplacer le passeur seul ou avec le loup, mouton ou chou
 *
 * @param etat L'état actuel (position des 4 intervenants)
 * @param symbole Le symbole représentant l'action: - "p" quand le passeur traverse seul
 *                                                  - "l" quand le passeur traverse avec le loup
 *                                                  - "m" quand le passeur traverse avec le mouton
 *                                                  - "c" quand le passeur traverse avec le chou
 * @return un Option du nouvel état après la transition
 */
def deltaLoupMoutonChou(etat: EtatLMC, symbole: String): Option[EtatLMC] = (etat, symbole) match
    // Le passeur traverse seul
    case ((gauche, droite), "p") if !estInterdit((gauche, droite)) && gauche.contains(P) => Some(((gauche - P, droite + P)))
    case ((gauche, droite), "p") if !estInterdit((gauche, droite)) && droite.contains(P) => Some(((gauche + P, droite - P)))

    // Le passeur traverse avec le loup
    case ((gauche, droite), "l") if !estInterdit((gauche, droite)) && gauche.contains(P) && gauche.contains(L) => Some(((gauche - P - L, droite + P + L)))
    case ((gauche, droite), "l") if !estInterdit((gauche, droite)) && droite.contains(P) && droite.contains(L) => Some(((gauche + P + L, droite - P - L)))

    // Le passeur traverse avec le mouton
    case ((gauche, droite), "m") if !estInterdit((gauche, droite)) && gauche.contains(P) && gauche.contains(M) => Some(((gauche - P - M, droite + P + M)))
    case ((gauche, droite), "m") if !estInterdit((gauche, droite)) && droite.contains(P) && droite.contains(M) => Some(((gauche + P + M, droite - P - M)))

    // Le passeur traverse avec le chou
    case ((gauche, droite), "c") if !estInterdit((gauche, droite)) && gauche.contains(P) && gauche.contains(C) => Some(((gauche - P - C, droite + P + C)))
    case ((gauche, droite), "c") if !estInterdit((gauche, droite)) && droite.contains(P) && droite.contains(C) => Some(((gauche + P + C, droite - P - C)))

    case (_, _) => None

@main def LoupMoutonChou =
    val ex = new AFD(Set("p", "l", "m", "c"), deltaLoupMoutonChou, etatInitial, Set(etatFinal))

    println(s"ex.solve() -> ${ex.solve()}")

    val lazySolution = ex.lazysolve()
    println(s"ex.lazysolve().take(2).toList -> ${lazySolution.take(2).toList}")

    val mot1: Mot[String] = List("m", "p", "c", "m", "l", "c", "m", "l", "c", "m", "l", "p", "m")
    println(s"ex.accept($mot1) -> ${ex.accept(mot1)}")
    val mot2: Mot[String] = List("p", "p", "p")
    println(s"ex.accept($mot2) -> ${ex.accept(mot2)}")
    //val mot3: Mot[String] = List("m", "p", "l", "m", "c", "p", "m")
    //println(s"ex.accept($mot3) -> ${ex.accept(mot3)}")
    //val mot4: Mot[String] = List("m", "p", "c", "m", "l", "p", "m")
    //println(s"ex.accept($mot4) -> ${ex.accept(mot4)}")
