// synonyme de type Mot pour représenter un mot qui correspond à une liste de type B
type Mot[+B] = List[B]

// synonyme de type Chemin pour représenter un chemin qui correspond à une liste de type B
type Chemin[+B] = List[B]

/** synonyme de type File pour représenter une file de proprité qui correspond à une liste de tuple contenant:
 *             - L'état actuel de type A
 *             - Le chemin actuel (renversé) représenté comme une liste de symboles de type B menant à cet état actuel
 *             - Le coût heuristique de cet état de type Double
 *             - L'ensemble des états déjà visités de type A
 */
type File[A,+B] = List[(A, Chemin[B], Double, Set[A])]

/**
 * Essaye d'évaluer une expression et retourne un Some contenant le résultat en cas de succès,
 * ou None si une exception est levée.
 *
 * @param a Une expression ou un calcul dont le résultat est de type A
 * @tparam A Le type de la valeur retournée
 * @return Une Option: Some contenant le résultat si aucune exception n'est levée,
 *         ou None en cas d'exception.
 */
def Try[A](a: => A): Option[A] = 
    try Some(a)
    catch case e: Exception => None
    //dans les slides Exception a la place de IllegalArgumentException

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
     * @param mot Le mot (List de symboles de type B) à vérifier
     * @return true si le mot est accepté, false sinon
     */
    def accept(mot: Mot[B]): Boolean =
        mot.foldLeft(Some(s): Option[A])((etatOption, symbole) => etatOption.flatMap(etat => delta(etat, symbole))).exists(F.contains)

    /**
     * Tente de vérifier si une chaîne de caractères représente un mot accepté par l'AFD,
     * après avoir converti cette chaîne en un mot à l'aide de la fonction parse.
     *
     * @param input La chaîne de caractères représentant le mot.
     * @param parse Une fonction prenant une chaîne en entrée et retournant un mot (List de symboles de type B).
     * @return true si le mot correspondant à la chaîne est accepté par l'automate, false sinon.
     *         Retourne également false si une exception est levée lors de l'exécution du parse.
     */
    def parseAccept[A](input: String, parse: String => Mot[B]): Boolean = 
        Try(parse(input)).map(accept).getOrElse(false)

    /**
     * Renvoie, à partir d’un état, toutes les paires (symbole, état) menant aux états adjacents
     *
     * @param etat L'état actuel de type A à partir duquel on cherche les adjacents
     * @return Un ensemble de paires (symbole de type B, état adjacent de type A) où delta(état actuel, symbole) = état adjacent
     */
    private def adjacence(etat: A): Set[(B, A)] =
        sigma.flatMap(symbole => delta(etat, symbole).map(nouvelEtat => (symbole, nouvelEtat)))

    /**
     * Renvoie tous les mots sans cycles qui conduisent à des états acceptés à partir de l'état initial
     * La recherche est basée sur une heuristique si celle-ci est donné en paramètre.
     *
     * @param heuristique Une fonction qui évalue le "coût" de type Double d'un état donné de type A
     *                    Un coût plus bas signifie que l'état est plus prometteur pour exploration
     *                    Par défaut, la fonction retourne 0
     * @return Une liste de mots (List de symboles de type B) qui mènent à un état accepteur
     */
    def solve(heuristique: A => Double = _ => 0): List[Mot[B]] =
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
        def recherche(file: File[A,B], solutions: List[Mot[B]]): List[Mot[B]] =
            file match
                case Nil => solutions
                case (etatActuel, chemin, _, visites) :: reste =>
                    /*Dans notre implémentation, nous avons choisi de ne pas utiliser directement la fonction accept pour vérifier si un chemin mène à un état accepteur.
                    En effet, nous avons déjà accès à l’état final du chemin à chaque itération, ce qui nous permet de vérifier directement si cet état est dans l’ensemble F.
                    Si nous avions utilisé accept, cela aurait été moins efficace, car il aurait fallu reconstruire le chemin dans l’ordre correct pour le parcourir. 
                    Si nécessaire, nous aurions pu appeler accept(chemin.reverse) pour vérifier cette condition, mais cela n’est pas nécessaire ici.*/
                    val nouvellesSolutions = if F.contains(etatActuel) then (chemin.reverse: Mot[B]) :: solutions
                                            else solutions
                    val nouvelleFile = adjacence(etatActuel).foldLeft(reste)((acc, transition) => transition match
                        case (symbole, etatAdjacent) if !visites.contains(etatAdjacent) && etatAdjacent != etatActuel =>
                            (etatAdjacent, symbole :: chemin, heuristique(etatAdjacent), visites + etatActuel) :: acc
                        case _ => acc
                    ).sortBy(_._3)
                    recherche(nouvelleFile, nouvellesSolutions)
        recherche(List((s, Nil, heuristique(s), Set())), Nil)

    /**
     * Renvoie une lazy list qui contient tous les mots sans cycles qui conduisent à des états acceptés à partir de l'état initial
     * La recherche est basée sur une heuristique si celle-ci est donné en paramètre.
     *
     * @param heuristique Une fonction qui évalue le "coût" d'un état donné de type A
     *                    Un coût plus bas signifie que l'état est plus prometteur pour exploration
     *                    Par défaut, la fonction retourne 0
     * @return Une LazyList de mots (List de symboles de type B) qui mènent à un état accepteur, générés paresseusement
     */
    def lazysolve(heuristique: A => Double = _ => 0): LazyList[Mot[B]] =
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
        def recherche(file: File[A,B]): LazyList[Mot[B]] =
            file match
                case Nil => LazyList.empty
                case (etatActuel, chemin, _, visites) :: reste =>
                    val nouvelleFile = adjacence(etatActuel).foldLeft(reste)((acc, transition) => transition match
                        case (symbole, etatAdjacent) if !visites.contains(etatAdjacent) && etatAdjacent != etatActuel =>
                            (etatAdjacent, symbole :: chemin, heuristique(etatAdjacent), visites + etatActuel) :: acc
                        case _ => acc
                    ).sortBy(_._3)
                    /*Dans notre implémentation, nous avons choisi de ne pas utiliser directement la fonction accept pour vérifier si un chemin mène à un état accepteur.
                    En effet, nous avons déjà accès à l’état final du chemin à chaque itération, ce qui nous permet de vérifier directement si cet état est dans l’ensemble F.
                    Si nous avions utilisé accept, cela aurait été moins efficace, car il aurait fallu reconstruire le chemin dans l’ordre correct pour le parcourir. 
                    Si nécessaire, nous aurions pu appeler accept(chemin.reverse) pour vérifier cette condition, mais cela n’est pas nécessaire ici.*/
                    if F.contains(etatActuel) then (chemin.reverse: Mot[B]) #:: recherche(nouvelleFile)
                    else recherche(nouvelleFile)
        recherche(List((s, Nil, heuristique(s), Set())))
