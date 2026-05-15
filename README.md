# Le roman québécois contemporain à l'épreuve de la lecture distanciée (2000-2021)

Données et code accompagnant l'article du même titre, à paraître dans la revue
*COnTEXTES*, dossier **« Lecture distante, lecture rapprochée et
intermédiaires ? »**, dirigé par Karol'Ann Boivin et Julien Lefort-Favreau.

**Auteurs de l'article :** Michel Biron (Université McGill) et Pascal Brissette (Université McGill).

**Auteur du code et responsable du dépôt :** Pascal Brissette.

**Contact :** *pascal.brissette@mcgill.ca*

---

## Contenu du dépôt

Ce dépôt suit un modèle minimaliste :

| Fichier | Description |
|---|---|
| `corpus_notices.csv` | Table d'ancrage du corpus : une ligne par roman, deux colonnes (numéro de notice, ISBN). |
| `methode.R` | Script R documenté qui expose, dans l'ordre de l'article, la démarche d'analyse — comment chaque résultat et chacune des deux figures ont été obtenus. |
| `figure_1_titres_par_annee.png` | Figure 1 de l'article (distribution annuelle des titres), produite par `methode.R`. |
| `figure_2_amour_amitie.png` | Figure 2 de l'article (fréquence relative des motifs « \\bamour » et « amitié »), produite par `methode.R`. |
| `README.md` | Le présent fichier. |

---

## Le corpus

L'article travaille sur un corpus d'environ 7 800 romans québécois pour adultes
et adolescents, parus entre 2000 et 2021. Ce corpus résulte d'une liste 
initiale de plus de 24 000 titres fournie par les
bibliothécaires de Bibliothèque et Archives nationales du Québec (BAnQ),
révisée et réduite d'environ les deux tiers par une équipe d'assistants de
recherche de l'Université McGill: Salomé Landry, Ophélie Proulx-Giraldeau, 
Marie Chartrand-Caulet, Guillaume Sirois et Marianne Ducharme, 
sous la supervision de Michel Biron.

### Structure de `corpus_notices.csv`

| Colonne | Description |
|---|---|
| `numero_notice` | Numéro de notice BAnQ (chaîne de caractères, zéros de tête conservés). Un même numéro peut couvrir plusieurs volumes d'une série ; il n'est donc pas unique. Une valeur est manquante. |
| `ISBN` | ISBN du livre tel que consigné dans la notice. Présent pour chaque ligne. Le format n'est pas homogène (ISBN-10, ISBN-13, et quelques valeurs non normalisées héritées du catalogue). |

Le fichier compte 7 813 lignes (une par roman du corpus analysé).


---

## Conditions de diffusion des données

BAnQ a autorisé la diffusion de la seule table à deux champs
(`corpus_notices.csv`). **Les notices complètes — titre, auteur, éditeur, lieu
d'édition, année, descripteurs de sujet — ne peuvent être rediffusées** et 
ne figurent donc pas dans ce dépôt.

En conséquence, `methode.R` **n'est pas exécutable en l'état**. Sa vocation
n'est pas la reproduction des résultats, mais l'exposition de la **méthode** :
il montre, pour chaque résultat de l'article, comment il a été obtenu. Le script
suppose un sous-répertoire `data/` contenant les tables de travail
(`livres.csv`, `sujets.csv`, `genres.csv`, `categories_geo.csv`) ; les colonnes
attendues sont déclarées en tête du fichier.

`livres.csv` et `sujets.csv` sont reconstituables à partir de
`corpus_notices.csv`, en interrogeant le catalogue de BAnQ. En revanche,
`genres.csv` et `categories_geo.csv` sont des **constructions propres au
projet** : le premier est dérivé par un pipeline de normalisation des genres,
le second est une classification géographique manuelle des lieux d'édition.
Ils ne sont pas reconstituables à partir des seules notices.

Les **agrégats statistiques** évoqués dans `methode.R` (nombres de titres par
segment éditorial, distributions annuelles de descripteurs, pentes de
tendance) ne constituent pas une reproduction de la base : ils peuvent être
librement diffusés, et sont versés dans les annexes statistiques qui
accompagnent l'article.

---

## Lecture de `methode.R`

Le script est organisé selon les trois sections de l'article :

- **Section I — Interroger une forme.** Distribution annuelle des titres ;
  figure 1.
- **Section II — Croissance de l'édition littéraire.** Décomposition de la
  croissance en neuf segments croisant le profil de persistance de l'éditeur
  (ponctuel, intermittent, pilier) et sa zone géographique (urbain, banlieue,
  régions) ; tableau 1 ; trajectoires des principales maisons.
- **Section III — D'amour et d'amitié.** Fréquence annuelle des motifs « amour »
  et « amitié » dans les descripteurs ; figure 2 ; tests de robustesse du
  chiasme par zone et par profil ; décomposition de la hausse de l'amitié.

Chaque bloc de calcul est commenté de manière à indiquer le résultat de
l'article qu'il documente. Les résultats sont rassemblés dans des objets
nommés (`tableau_1`, `decomposition_amitie`, `robustesse_zone`, etc.) plutôt
qu'imprimés ; la construction des deux figures de l'article y figure également.

---

## Limites

- `corpus_notices.csv` fige le corpus dans l'état où il a servi à l'article. Le
  catalogue de BAnQ, lui, continue d'évoluer : une reconstitution ultérieure
  des notices pourra donc différer à la marge de celle utilisée ici.

---

## Annexes de l'article

Trois annexes statistiques accompagnent l'article :

- la table des éditeurs du corpus
- la distribution annuelle des descripteurs
principaux
- une vue synthétique des sous-corpus thématiques

---

## Citation suggérée

Biron, M. et Brissette, P. (à paraître). « Le roman québécois contemporain à
l'épreuve de la lecture distanciée (2000-2021) ». *COnTEXTES*, dossier
« Lecture distante, lecture rapprochée et intermédiaires ? », dir.
K.-A. Boivin et J. Lefort-Favreau.

Pour le présent dépôt :

> Brissette, P. (2026). *Données et code de « Le roman québécois contemporain
> à l'épreuve de la lecture distanciée (2000-2021) »* \[jeu de données\].

---

## Licence

- **`corpus_notices.csv`** : les numéros de notice et les ISBN proviennent du
  catalogue de Bibliothèque et Archives nationales du Québec (BAnQ). Leur
  rediffusion dans ce dépôt a été autorisée par BAnQ ; toute réutilisation doit
  mentionner la source. *(Licence précise à confirmer avec BAnQ avant dépôt.)*
- **`methode.R`, `README.md` et les figures** : *
  Creative Commons Attribution 4.0 International — CC BY 4.0.*
