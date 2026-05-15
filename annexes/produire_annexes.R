# =============================================================================
# Production des annexes de l'article
#   Le roman quebecois contemporain a l'epreuve de la lecture distanciee
#
# Ce script genere les trois annexes destinees a accompagner l'article :
#   Annexe A — Table des editeurs du corpus (304 editeurs quebecois)
#   Annexe B — Distribution annuelle des descripteurs principaux
#   Annexe C — Sous-corpus thematiques mentionnes dans l'article
#
# Chaque annexe est produite en deux formats : un fichier .csv (exploitation)
# et un fichier .md (lecture humaine).
#
# Pre-requis : memes fichiers de donnees que analyse.R. Le script suppose qu'il
# est execute depuis le repertoire annexes/, le repertoire data/ se trouvant un
# niveau au-dessus.
#
# Pour executer : Rscript produire_annexes.R   (depuis le repertoire annexes/)
# =============================================================================

suppressMessages(library(data.table))

DATA <- ".."  # les fichiers de donnees sont dans ../data/

livres  <- fread(file.path(DATA, "data", "livres.csv"))
sujets  <- fread(file.path(DATA, "data", "sujets.csv"))
genres  <- fread(file.path(DATA, "data", "genres.csv"))
cat_geo <- fread(file.path(DATA, "data", "categories_geo.csv"))

ids_annee <- livres[, .(AR_id, Annee = Annee_source)]

# Petit utilitaire : ecrire une data.table en table Markdown.
ecrire_markdown <- function(dt, fichier, titre, note = NULL) {
  con <- file(fichier, open = "w", encoding = "UTF-8")
  writeLines(paste0("# ", titre), con)
  writeLines("", con)
  if (!is.null(note)) { writeLines(note, con); writeLines("", con) }
  writeLines(paste0("| ", paste(names(dt), collapse = " | "), " |"), con)
  writeLines(paste0("|", paste(rep("---", ncol(dt)), collapse = "|"), "|"), con)
  for (i in seq_len(nrow(dt))) {
    writeLines(paste0("| ", paste(as.character(dt[i]), collapse = " | "), " |"), con)
  }
  close(con)
}


# =============================================================================
# ANNEXE A — TABLE DES EDITEURS DU CORPUS
# =============================================================================
# Une ligne par editeur quebecois du corpus (les editeurs hors Quebec sont
# exclus, conformement au perimetre de la section II de l'article). Documente
# la cartographie editoriale : profil de persistance, zone geographique, lieu
# d'edition principal, empan d'activite et volume de titres.

lieux_qc  <- cat_geo[categorie_geo != "hors_qc", Lieu_editeur]
livres_qc <- livres[Lieu_editeur %in% lieux_qc]

# Profil de persistance : nombre d'annees distinctes de publication.
annexe_A <- livres_qc[, .(
  premiere_annee = min(Annee_source),
  derniere_annee = max(Annee_source),
  annees_actives = uniqueN(Annee_source),
  titres         = .N
), by = Editeur]

annexe_A[, profil := fcase(
  annees_actives <= 5,  "Ponctuel",
  annees_actives <= 10, "Intermittent",
  annees_actives >= 11, "Pilier"
)]

# Zone dominante : zone ou l'editeur a publie le plus de titres.
geo <- merge(livres_qc, cat_geo, by = "Lieu_editeur", all.x = TRUE)
geo[, zone := fcase(
  categorie_geo == "urbain",   "Urbain",
  categorie_geo == "banlieue", "Banlieue",
  categorie_geo == "région",   "Régions"
)]
ed_zone  <- geo[, .(n = .N), by = .(Editeur, zone)]
setorder(ed_zone, Editeur, -n, zone)
zone_dom <- ed_zone[, .SD[1], by = Editeur][, .(Editeur, zone = zone)]
annexe_A <- merge(annexe_A, zone_dom, by = "Editeur")

# Lieu d'edition principal : le lieu ou l'editeur a publie le plus de titres.
ed_lieu  <- livres_qc[, .(n = .N), by = .(Editeur, Lieu_editeur)]
setorder(ed_lieu, Editeur, -n, Lieu_editeur)
lieu_dom <- ed_lieu[, .SD[1], by = Editeur][, .(Editeur, lieu_edition = Lieu_editeur)]
annexe_A <- merge(annexe_A, lieu_dom, by = "Editeur")

setorder(annexe_A, -titres, Editeur)
annexe_A <- annexe_A[, .(Editeur, profil, zone, lieu_edition,
                         premiere_annee, derniere_annee, annees_actives, titres)]
setnames(annexe_A,
         c("Editeur", "Profil", "Zone", "Lieu d'edition",
           "Premiere annee", "Derniere annee", "Annees actives", "Titres"))

fwrite(annexe_A, "annexe_A_editeurs.csv")
ecrire_markdown(
  annexe_A, "annexe_A_editeurs.md",
  "Annexe A — Table des editeurs du corpus (2000-2021)",
  paste(
    "Les 304 editeurs quebecois du corpus (les editeurs hors Quebec sont exclus).",
    "Profil de persistance : Ponctuel = 1 a 5 annees distinctes de publication ;",
    "Intermittent = 6 a 10 ; Pilier = 11 et plus. Zone : zone dominante de",
    "l'editeur (Urbain = Montreal et Quebec-ville ; Banlieue = RMR de Montreal et",
    "de Quebec ; Regions = autres municipalites quebecoises).", sep = " "))

cat(sprintf("Annexe A : %d editeurs.\n", nrow(annexe_A)))
cat(sprintf("  Pilier : %d | Intermittent : %d | Ponctuel : %d\n",
            annexe_A[Profil == "Pilier", .N],
            annexe_A[Profil == "Intermittent", .N],
            annexe_A[Profil == "Ponctuel", .N]))


# =============================================================================
# ANNEXE B — DISTRIBUTION ANNUELLE DES DESCRIPTEURS PRINCIPAUX
# =============================================================================
# Tableau croisant les annees (2000-2021) et les descripteurs analyses dans la
# section III. Pour chaque descripteur (ou motif), la valeur est le nombre de
# livres du corpus portant ce descripteur cette annee-la (un livre compte au
# plus une fois par descripteur). Documente le chiasme thematique.

annees <- data.table(Annee = 2000:2021)

# Comptage annuel d'un ensemble d'identifiants de livres.
.compte_annuel <- function(ids) {
  dt <- ids_annee[AR_id %in% ids, .(n = uniqueN(AR_id)), by = Annee]
  dt <- dt[annees, on = "Annee"]
  dt[is.na(n), n := 0L]
  setorder(dt, Annee)
  dt$n
}

# Motif "amour" : tout descripteur ou un mot debute par "amour" ("amour",
# "amours", "amoureux", "amoureuse"), la balise \\b ecartant "Samourai" ou
# "Kamouraska". Meme regex que dans analyse.R.
ids_amour  <- sujets[grepl("\\bamour", Sujets, ignore.case = TRUE, perl = TRUE), unique(AR_id)]
# Motif "amitie" : agrege les huit descripteurs contenant "amitie".
ids_amitie <- sujets[grepl("amitié",     Sujets, ignore.case = TRUE, perl = TRUE), unique(AR_id)]

annexe_B <- data.table(
  Annee                 = annees$Annee,
  `Total corpus`        = .compte_annuel(livres$AR_id),
  `Total indexes`       = .compte_annuel(sujets[, unique(AR_id)]),
  `Amour (motif)`       = .compte_annuel(ids_amour),
  `Amitie (motif)`      = .compte_annuel(ids_amitie)
)

# Les huit descripteurs "amitie" pris isolement.
desc_amitie <- sujets[grepl("amitié", Sujets, ignore.case = TRUE, perl = TRUE),
                      unique(Sujets)]
# Ordre : du plus au moins frequent.
desc_amitie <- sujets[Sujets %in% desc_amitie, .(n = uniqueN(AR_id)), by = Sujets][
  order(-n), Sujets]
for (d in desc_amitie) {
  annexe_B[[d]] <- .compte_annuel(sujets[Sujets == d, unique(AR_id)])
}

# Sous-genre "litterature de filles" et descripteurs co-occurrents du
# sous-corpus extra-generique.
annexe_B[["Litterature de fille (sous-genre)"]] <-
  .compte_annuel(genres[Sous_genre == "littérature de filles", unique(AR_id)])
for (d in c("Jeunes femmes", "Femmes d'âge moyen", "Mères et filles")) {
  annexe_B[[d]] <- .compte_annuel(sujets[Sujets == d, unique(AR_id)])
}

fwrite(annexe_B, "annexe_B_descripteurs.csv")
ecrire_markdown(
  annexe_B, "annexe_B_descripteurs.md",
  "Annexe B — Distribution annuelle des descripteurs principaux (2000-2021)",
  paste(
    "Nombre de livres du corpus portant chaque descripteur, par annee. Le motif",
    "\"Amour\" agrege les descripteurs captes par la regex amours*\\b ; le motif",
    "\"Amitie\" agrege les huit descripteurs contenant le mot. \"Litterature de",
    "fille\" renvoie au sous-genre. Les colonnes \"Total corpus\" et \"Total",
    "indexes\" fournissent les denominateurs (corpus complet ; livres ayant au",
    "moins un descripteur).", sep = " "))

cat(sprintf("Annexe B : %d annees x %d colonnes.\n", nrow(annexe_B), ncol(annexe_B)))


# =============================================================================
# ANNEXE C — SOUS-CORPUS MENTIONNES DANS L'ARTICLE
# =============================================================================
# Vue synthetique des principaux sous-corpus discutes dans la section III :
# taille totale, taille par triennale de debut et de fin, pente de la
# trajectoire annuelle (regression lineaire), et editeurs principaux.

ids_af  <- sujets[Sujets == "Amitié féminine",  unique(AR_id)]
ids_am  <- sujets[Sujets == "Amitié masculine", unique(AR_id)]
ids_ldf <- genres[Sous_genre == "littérature de filles", unique(AR_id)]
ids_afhors <- setdiff(ids_af, ids_ldf)

# Pour un sous-corpus donne : tailles, pente lm, R2, editeurs principaux.
.profil_sous_corpus <- function(ids, etiquette) {
  serie <- ids_annee[AR_id %in% ids, .(n = uniqueN(AR_id)), by = Annee]
  serie <- serie[annees, on = "Annee"]
  serie[is.na(n), n := 0L]
  fit <- lm(n ~ Annee, data = serie)
  top <- livres[AR_id %in% ids, .(n = uniqueN(AR_id)), by = Editeur][order(-n)]
  top4 <- top[seq_len(min(4, .N))]
  data.table(
    `Sous-corpus`        = etiquette,
    `Taille totale`      = length(ids),
    `Taille 2000-2002`   = ids_annee[AR_id %in% ids & Annee %in% 2000:2002, uniqueN(AR_id)],
    `Taille 2019-2021`   = ids_annee[AR_id %in% ids & Annee %in% 2019:2021, uniqueN(AR_id)],
    `Pente (livres/an)`  = round(coef(fit)[2], 2),
    `R2`                 = round(summary(fit)$r.squared, 2),
    `Editeurs principaux` = paste(sprintf("%s (%d)", top4$Editeur, top4$n),
                                  collapse = " ; ")
  )
}

annexe_C <- rbindlist(list(
  .profil_sous_corpus(ids_af,     "Amitie feminine (descripteur)"),
  .profil_sous_corpus(ids_am,     "Amitie masculine (descripteur)"),
  .profil_sous_corpus(ids_ldf,    "Litterature de fille (sous-genre)"),
  .profil_sous_corpus(ids_afhors, "Amitie feminine hors litterature de fille")
))

fwrite(annexe_C, "annexe_C_sous_corpus.csv")
ecrire_markdown(
  annexe_C, "annexe_C_sous_corpus.md",
  "Annexe C — Sous-corpus mentionnes dans l'article",
  paste(
    "Vue synthetique des sous-corpus thematiques de la section III. La taille",
    "est le nombre de livres distincts ; les tailles triennales comptent les",
    "livres parus durant chaque tranche de trois ans. La pente et le R2",
    "proviennent d'une regression lineaire du nombre annuel de livres sur",
    "l'annee. Les editeurs principaux sont les quatre maisons ayant publie le",
    "plus de titres du sous-corpus (nombre de titres entre parentheses).",
    sep = " "))

cat(sprintf("Annexe C : %d sous-corpus.\n", nrow(annexe_C)))
cat("\nAnnexes produites : 3 fichiers .csv et 3 fichiers .md.\n")
