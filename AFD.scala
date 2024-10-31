// Je sais pas si c'est bien fait
type Mot[B] = List[B]
type Chemin[B] = List[B]

/**
 * Une classe représentant un Automate Fini Déterministe (AFD)
 * 
 * @constructor crée un nouveau AFD avec sigma, delta, s, et F
 * @param sigma L'alphabet fini qui est un ensemble de type B
 * @param delta La fonction de transition, qui en fonction de l'état actuel de type A et d'un symbole de type B qui determine l'éventuel prochain état de type Option[A]
 *              //Note: pour représenter des transitions manquantes il faudra ajouté un état interdit sink afin que la fonction soit totale
 * @param s L'état initial de l'AFD, de type A
 * @param F L'ensemble des états accepteurs, de type A, qui est un sous ensemble des états
 * @tparam A Le type représentant les états
 * @tparam B Le type représentant les symboles composant l'alphabet
 */
// voir si ca doit etre une class/case class/trait/...
class AFD[A, B](sigma: Set[B], delta: (A, B) => Option[A], s: A, F: Set[A]):
    /**
     * Vérifie si un mot donné est accepté par l'AFD
     *
     * @param mot Le mot (séquence de symboles de type B) à vérifier
     * @return true si le mot est accepté, false sinon
     */
    def accept(mot: Mot[B]): Boolean =
        //F.contains(mot.foldLeft(s)((etat, symbole) => delta(etat, symbole)))
        mot.foldLeft(Option(s))((etat, symbole) => etat.flatMap(etat => delta(etat, symbole))).exists(F.contains)

    /**
     * Renvoie, à partir d’un état, toutes les paires (symbole, état) menant aux états adjacents
     *
     * @param etat L'état actuel de type A à partir duquel on cherche les adjacents
     * @return Un ensemble de paires (symbole de type B, état adjacent de type A) où delta(état actuel, symbole) = état adjacent
     */
    //mettre en private ou qqch du genre, car utile que pour les solve/lazysolve
    private def adjacence(etat: A): Set[(B, A)] =
        //sigma.flatMap(symbole => Some((symbole, delta(etat, symbole))))
        sigma.flatMap(symbole => delta(etat, symbole).map(nouvelEtat => (symbole, nouvelEtat)))

    /**
     * Renvoie tous les mots sans cycles qui conduisent à des états acceptés à partir de l'état initial
     *
     * @return Une liste de mots (séquence de symboles de type B) qui mène à un état acceptant
     */
    def solve(): List[Mot[B]] = solveHeuristique((x) => 0)
    // Peeutetre généraliser solve en fournissant un moyen de sort, afin de ne pas sort pour ce solve vu qu'inutile ?

    // Faudra trouver laquelle des implémentations est meilleur
    // surement celle avec tail rec car le prof adore ca, vu que scala est censé optimizer
    /**
     * Renvoie tous les mots sans cycles qui conduisent à des états acceptés à partir de l'état initial
     * La recherche est basée sur une heuristique
     *
     * @param heuristique Une fonction qui évalue le "coût" de type Double d'un état donné de type A
     *                    Un coût plus bas signifie que l'état est plus prometteur pour exploration
     * @return Une liste de mots (séquence de symboles de type B) qui mènent à un état accepteur
     */
    def solveHeuristique(heuristique: A => Double): List[Mot[B]] =
        /**
         * Fonction récursive pour explorer les états et les chemins
         * 
         * @param file La file de priorité, représentée comme une liste triée, contenant les noeuds à explorer
         *             Chaque noeud est un tuple contenant :
         *             - L'état actuel de type A
         *             - Le chemin actuel (renversé) représenté comme une liste de symboles de type B menant à cet état actuel
         *             - Le coût heuristique de cet état de type Double
         *             - L'ensemble des états déjà visités de type A (pour éviter les cycles)
         * @param solutions La liste accumulée des solutions trouvées (chemins qui mènent à un état accepteur)
         * @return La liste des solutions
         */
        @annotation.tailrec
        def recherche(file: List[(A, Chemin[B], Double, Set[A])], solutions: List[Mot[B]]): List[Mot[B]] =
            file match
                case Nil => solutions
                case (etatActuel, chemin, _, visites) :: reste =>
                    val nouveauxNoeuds = adjacence(etatActuel).collect {
                        case (symbole, etatAdjacent) if !visites.contains(etatAdjacent) && etatAdjacent != etatActuel =>
                            (etatAdjacent, symbole :: chemin, heuristique(etatAdjacent), visites + etatActuel)
                    }.toList
                    val nouvellesSolutions = if F.contains(etatActuel) then (chemin.reverse: Mot[B]) :: solutions
                                            else solutions
                    val nouvelleFile = (reste ::: nouveauxNoeuds).sortBy(_._3) // surement pas la meilleur approche pour l'efficacité
                    recherche(nouvelleFile, nouvellesSolutions)
        recherche(List((s, Nil, heuristique(s), Set())), Nil)


    /**
     * Renvoie tous les mots sans cycles qui conduisent à des états acceptés à partir de l'état initial
     * La recherche est basée sur une heuristique
     *
     * @param heuristique Une fonction qui évalue le "coût" d'un état donné de type A
     *                    Un coût plus bas signifie que l'état est plus prometteur pour exploration
     * @return Une liste de mots (séquence de symboles de type B) qui mènent à un état accepteur
     */
    def solveHeuristique2(heuristique: A => Double): List[Mot[B]] =
        /**
         * Fonction récursive pour explorer les états et les chemins à partir de la file de priorité file
         *
         * @param file La file de priorité, représentée comme une liste triée, contenant les noeuds à explorer
         *             Chaque noeud est un tuple contenant :
         *             - L'état actuel de type A
         *             - Le chemin actuel (renversé) représenté comme une liste de symboles de type B menant à cet état actuel
         *             - Le coût heuristique de cet état de type Double
         *             - L'ensemble des états déjà visités de type A (pour éviter les cycles)
         * @return La list de solutions
         */
        def recherche(file: List[(A, Chemin[B], Double, Set[A])]): List[Mot[B]] =
            file match
                case Nil => List.empty
                case (etatActuel, chemin, _, visites) :: reste =>
                    val nouveauxNoeuds = adjacence(etatActuel).collect {
                        case (symbole, etatAdjacent) if !visites.contains(etatAdjacent) && etatAdjacent != etatActuel =>
                            (etatAdjacent, symbole :: chemin, heuristique(etatAdjacent), visites + etatActuel)
                    }.toList
                    val nouvelleFile = (reste ::: nouveauxNoeuds).sortBy(_._3) // surement pas la meilleur approche pour l'efficacité
                    if F.contains(etatActuel) then (chemin.reverse: Mot[B]) :: recherche(nouvelleFile)
                    else recherche(nouvelleFile)
        recherche(List((s, Nil, heuristique(s), Set())))

    /**
     * Renvoie une lazy list qui contient tous les mots sans cycles qui conduisent à des états acceptés à partir de l'état initial
     *
     * @return Une LazyList de mots (séquence de symboles de type B) qui mène à un état acceptant
     */
    def lazysolve(): LazyList[Mot[B]] = lazysolveHeuristique((x) => 0)

    // Pas de possibilité d'utiliser tail rec sinon ca fait la "récursion entiere" avant de retourner la lazylist, donc ca n'est pas une évaluation paresseuse
    /**
     * Renvoie une lazy list qui contient tous les mots sans cycles qui conduisent à des états acceptés à partir de l'état initial
     * La recherche est basée sur une heuristique
     *
     * @param heuristique Une fonction qui évalue le "coût" d'un état donné de type A
     *                    Un coût plus bas signifie que l'état est plus prometteur pour exploration
     * @return Une LazyList de mots (séquence de symboles de type B) qui mènent à un état accepteur, générés paresseusement
     */
    def lazysolveHeuristique(heuristique: A => Double): LazyList[Mot[B]] =
        /**
         * Fonction récursive pour explorer les états et les chemins de manière paresseuse à partir de la file de priorité file
         *
         * @param file La file de priorité, représentée comme une liste triée, contenant les noeuds à explorer
         *             Chaque noeud est un tuple contenant :
         *             - L'état actuel de type A
         *             - Le chemin actuel (renversé) représenté comme une liste de symboles de type B menant à cet état actuel
         *             - Le coût heuristique de cet état de type Double
         *             - L'ensemble des états déjà visités de type A (pour éviter les cycles)
         * @return Une LazyList de solutions générées paresseusement
         */
        def recherche(file: List[(A, Chemin[B], Double, Set[A])]): LazyList[Mot[B]] =
            file match
                case Nil => LazyList.empty
                case (etatActuel, chemin, _, visites) :: reste =>
                    val nouveauxNoeuds = adjacence(etatActuel).collect {
                        case (symbole, etatAdjacent) if !visites.contains(etatAdjacent) && etatAdjacent != etatActuel =>
                            (etatAdjacent, symbole :: chemin, heuristique(etatAdjacent), visites + etatActuel)
                    }.toList
                    val nouvelleFile = (reste ::: nouveauxNoeuds).sortBy(_._3) // surement pas la meilleur approche pour l'efficacité
                    if F.contains(etatActuel) then (chemin.reverse: Mot[B]) #:: recherche(nouvelleFile)
                    else recherche(nouvelleFile)
        recherche(List((s, Nil, heuristique(s), Set())))
