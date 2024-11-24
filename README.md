# Programmation fonctionnelle : Solveur d'automates

## Organisation du code

Le code a été réparti en plusieurs fichiers pour une meilleure compréhension :

* **AFD.scala** contient ce qui concerne la définition d'un AFD
* **Somme4.scala** contient l'AFD qui, à parti de 0, donne toutes les séquences de +1 ou +2 qui nous donnent la somme de 4
* **ChainesBinairesImpaires.scala** contient l'AFD qui accepte les chaines binaires impaires
* **LoupMoutonChou.scala** contient l'AFD pour le problème du loup, du mouton et du chou
* **Taquin.scala** contient ce qui concerne le jeu du taquin

## Exécution du code

Afin de simplifier le lancement du code, nous avons fourni un Makefile. Il suffit de faire `make` pour exécuter les 4 exemples d'automates ou de fournir le nom d'un des automates si nous sommes intéressés par un automate précis, par exemple `make Taquin`.

Le Makefile exécute les commandes suivantes :

```
scala AFD.Scala Somme4.scala
scala AFD.Scala ChainesBinairesImpaires.scala
scala AFD.Scala LoupMoutonChou.scala
scala AFD.Scala Taquin.scala
```

## Utilisation du code

Dans la suite, nous allons expliquer comment utiliser le code fourni au travers de l'exemple de l'AFD qui, à partir de 0, nous donne toutes les séquences de +1 ou +2 qui nous donnent la somme de 4.

1. Il faut créé un ADT qui représentera les états de l'AFD.
    ```
    sealed trait EtatSomme4
    case object S0 extends EtatSomme4
    case object S1 extends EtatSomme4
    case object S2 extends EtatSomme4
    case object S3 extends EtatSomme4
    case object S4 extends EtatSomme4
    ```

2. La fonction de transition doit être définie. Cette fonction prendra en argument un état et un symbole et retourne un Option du nouvel état après la transition. L'Option permet d'avoir une fonction totale.
    ```
    def deltaSomme4(etat: EtatSomme4, symbole: Int): Option[EtatSomme4] = (etat, symbole) match
        case (S0, 1) => Some(S1)
        case (S0, 2) => Some(S2)
        case (S1, 1) => Some(S2)
        case (S1, 2) => Some(S3)
        case (S2, 1) => Some(S3)
        case (S2, 2) => Some(S4)
        case (S3, 1) => Some(S4)
        case (_, _)  => None
    ```

3. Si l'on veut créer son propre automate, il faut instancier un AFD avec comme paramètre l'alphabet fini, la fonction de transition, l'état initial et l'ensemble des états accepteurs.
    ```
    val ex = new AFD(Set(1, 2), deltaSomme4, S0, Set(S4))
    ```

4. On peut utiliser les fonctions définis dans la classe ADT:
    * accept: prend en paramètre un mot qui correspond à une liste.
        ```
        val mot: Mot[Int] = List(1,1,1,1)
        ex.accept(mot)
        ```
    * solve: optionnel: prend en paramètre une heuristique qui est une fonction qui génère un Double à partir d'un état de l'ADT.
        ```
        ex.solve()
        ```
    * lazysolve: optionnel: prend en paramètre une heuristique qui est une fonction qui génère un Double à partir d'un état de l'ADT.
        ```
        ex.lazysolve().take(1).toList
        ```
