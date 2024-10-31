SCALA = scala

FILES = Somme4.scala ChainesBinairesImpaires.scala LoupMoutonChou.scala Taquin.scala

all: run_all

run_all: Somme4 ChainesBinairesImpaires LoupMoutonChou Taquin

Somme4:
	$(SCALA) AFD.scala Somme4.scala

ChainesBinairesImpaires:
	$(SCALA) AFD.scala ChainesBinairesImpaires.scala

LoupMoutonChou:
	$(SCALA) AFD.scala LoupMoutonChou.scala

Taquin:
	$(SCALA) AFD.scala Taquin.scala
