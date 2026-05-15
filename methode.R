# =============================================================================
# Le roman québécois contemporain à l'épreuve de la lecture distanciée
# (2000-2021) — Démarche d'analyse
#
# Auteur du code : Pascal Brissette (Université McGill)
# Article pour la revue COnTEXTES. Co-auteurs: Michel Biron et Pascal Brissette.
# Dossier "Lecture distante, lecture rapprochée et intermédiaires?",
# piloté par Karol'Ann Boivin et Julien Lefort-Favreau
# -----------------------------------------------------------------------------
#
#   Le présent fichier expose la démarche d'analyse de l'article : il montre
#   comment chaque résultat a été obtenu. Il n'est pas conçu
#   pour la reproduction systématique de ces résultats.
#
#   Bibliothèque et Archives nationales du Québec (BAnQ) n'autorise pas la
#   diffusion intégrale des données : seule la table à deux
#   champs (corpus_notices.csv : numero de notice et ISBN) peut être publiée. 
#   Les tables mobilisées ci-dessous ne sont donc pas jointes au dépôt
#   et ce script n'est pas exécutable en l'état. Il documente la méthode.
#
#   Tables de travail impliquées, mais non disponibles (sous-répertoire data/) :
#     livres.csv          un roman par ligne : AR_id, Annee_source, Editeur,
#                         Lieu_editeur
#     sujets.csv          descripteurs de sujet BAnQ : AR_id, Sujets
#     genres.csv          genres et sous-genres par livre : AR_id, Sous_genre
#                         (fichier dérivé, propre au projet, non reconstituable
#                         à partir des seules notices)
#     categories_geo.csv  classification des lieux d'édition : Lieu_editeur,
#                         categorie_geo (urbain, banlieue, région, hors_qc)
# =============================================================================

library(data.table)
library(ggplot2)
library(scales)

theme_set(theme_minimal(base_size = 13))

# Palette Okabe-Ito
PAL_AMOUR  <- "#0072B2"
PAL_AMITIE <- "#E69F00"
PAL_LM     <- "#CC79A7"

livres  <- fread("data/livres.csv")
sujets  <- fread("data/sujets.csv")
genres  <- fread("data/genres.csv")
cat_geo <- fread("data/categories_geo.csv")

# Table de référence : un livre, une année.
ids_annee <- livres[, .(AR_id, Annee = Annee_source)]


# =============================================================================
# SECTION I — INTERROGER UNE FORME
# =============================================================================
# La question initiale est simple : combien de romans québécois paraissent
# chaque année ? La figure 1 porte le décompte annuel des titres du corpus,
# accompagné d'une droite de tendance linéaire et d'une courbe de lissage.

dt_annee <- livres[, .(n_titres = .N), by = Annee_source][order(Annee_source)]

figure_1 <- ggplot(dt_annee, aes(x = Annee_source, y = n_titres)) +
  geom_col(fill = "#56B4E9", alpha = 0.85, width = 0.7) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x,
              colour = PAL_LM, linetype = "dashed", linewidth = 0.8) +
  geom_smooth(method = "loess", se = TRUE, formula = y ~ x,
              colour = PAL_AMITIE, fill = PAL_AMITIE, linewidth = 1,
              span = 0.75, alpha = 0.2) +
  scale_x_continuous(breaks = seq(2000, 2021, 5)) +
  labs(x = NULL, y = "Nombre de titres", title = "Titres publiés par année")

ggsave("figure_1_titres_par_annee.png", figure_1, width = 7, height = 4.5, dpi = 300)


# =============================================================================
# SECTION II — CROISSANCE DE L'ÉDITION LITTÉRAIRE
# =============================================================================
# La croissance globale est décomposée selon deux axes : le profil de
# persistance de l'éditeur dans le roman et la zone géographique où il publie.
# Les éditeurs hors Québec sont exclus de cette section.

lieux_qc  <- cat_geo[categorie_geo != "hors_qc", Lieu_editeur]
livres_qc <- livres[Lieu_editeur %in% lieux_qc]

# Profil de persistance : nombre d'années distinctes de publication dans le
# corpus (Ponctuel = 1-5 ans ; Intermittent = 6-10 ; Pilier = 11 et plus).
dt_editeurs <- livres_qc[, .(
  premiere_annee = min(Annee_source),
  derniere_annee = max(Annee_source),
  n_titres_total = .N,
  n_annees_actif = uniqueN(Annee_source)
), by = Editeur]
dt_editeurs[, profil := fcase(
  n_annees_actif <= 5,  "Ponctuel",
  n_annees_actif <= 10, "Intermittent",
  n_annees_actif >= 11, "Pilier"
)]

# Zone dominante de l'éditeur : celle où il a publié le plus de titres.
geo <- merge(livres_qc, cat_geo, by = "Lieu_editeur", all.x = TRUE)
geo[, zone := fcase(
  categorie_geo == "urbain",   "Urbain",
  categorie_geo == "banlieue", "Banlieue",
  categorie_geo == "région",   "Regions"
)]
ed_zone <- geo[, .(n = .N), by = .(Editeur, zone)]
setorder(ed_zone, Editeur, -n, zone)
dt_editeurs <- merge(dt_editeurs,
                     ed_zone[, .SD[1], by = Editeur][, .(Editeur, zone_dom = zone)],
                     by = "Editeur")

# Croisement profil x zone : neuf segments.
dt_editeurs[, segment := paste(profil, zone_dom, sep = "_")]

# Chaque titre herite du segment de son éditeur ; série annuelle par segment,
# complétée pour les années sans titre.
dt_livres_seg <- merge(livres_qc, dt_editeurs[, .(Editeur, segment)], by = "Editeur")
dt_an_seg <- dt_livres_seg[, .(n = .N), by = .(Annee_source, segment)]
dt_an_seg <- dt_an_seg[CJ(Annee_source = 2000:2021, segment = unique(dt_editeurs$segment)),
                       on = .(Annee_source, segment)]
dt_an_seg[is.na(n), n := 0L]

# Tableau 1 : la croissance d'un segment est la différence entre sa moyenne
# triennale de fin (2019-2021) et celle de debut (2000-2002).
tableau_1 <- merge(
  dt_an_seg[Annee_source %in% 2000:2002, .(moy_debut = mean(n)), by = segment],
  dt_an_seg[Annee_source %in% 2019:2021, .(moy_fin   = mean(n)), by = segment],
  by = "segment")
tableau_1[, difference := moy_fin - moy_debut]
croissance_totale <- sum(tableau_1$difference)
tableau_1[, contribution_pct := round(100 * difference / croissance_totale, 1)]
setorder(tableau_1, -contribution_pct)

# Contribution agrégée par macro-profil : les éditeurs piliers portent la plus
# grande part de la croissance.
tableau_1[, macro_profil := sub("_.*", "", segment)]
contribution_profil <- tableau_1[, .(
  difference       = sum(difference),
  contribution_pct = sum(contribution_pct)
), by = macro_profil]

# Trajectoire de chaque éditeur : moyennes triennales de début et de fin.
moyennes_triennales <- function(editeurs_vec) {
  prod <- livres_qc[Editeur %in% editeurs_vec, .(n = .N), by = .(Editeur, Annee_source)]
  prod <- prod[CJ(Editeur = editeurs_vec, Annee_source = 2000:2021),
               on = .(Editeur, Annee_source)]
  prod[is.na(n), n := 0L]
  out <- merge(
    prod[Annee_source %in% 2000:2002, .(moy_debut = mean(n)), by = Editeur],
    prod[Annee_source %in% 2019:2021, .(moy_fin   = mean(n)), by = Editeur],
    by = "Editeur")
  out[, diff := moy_fin - moy_debut]
}

# Pilier_Urbain : on distingue les éditeurs déjà actifs en 2000-2002 ("anciens")
# de ceux entrés en 2003 ou après ("nouveaux"), pour mesurer la part de la
# croissance urbaine qui revient aux nouveaux entrants.
piliers_urbains <- merge(dt_editeurs[segment == "Pilier_Urbain"],
                         moyennes_triennales(dt_editeurs[segment == "Pilier_Urbain", Editeur]),
                         by = "Editeur")
piliers_urbains[, statut := fifelse(premiere_annee <= 2002, "Ancien", "Nouveau")]
contribution_anciens_nouveaux <- piliers_urbains[, .(diff = sum(diff)), by = statut]

# Trajectoire detaillée des trois segments piliers (urbain, banlieue, régions),
# qui sous-tend les exemples nominatifs de la section II.
piliers_banlieue <- merge(dt_editeurs[segment == "Pilier_Banlieue"],
                          moyennes_triennales(dt_editeurs[segment == "Pilier_Banlieue", Editeur]),
                          by = "Editeur")
piliers_regions  <- merge(dt_editeurs[segment == "Pilier_Regions"],
                          moyennes_triennales(dt_editeurs[segment == "Pilier_Regions", Editeur]),
                          by = "Editeur")


# =============================================================================
# SECTION III — D'AMOUR ET D'AMITIÉ
# =============================================================================
# Chaque notice porte des descripteurs de sujet attribués par BAnQ.
# On interroge ici deux thèmes massivement exploités — l'amour
# et l'amitié — et le chassé-croisé de leur évolution.

# Motifs recherchés par expression régulière, insensible à la casse :
#   "\\bamour" capte tout descripteur commençant par "amour" (la balise
#   \\b écarte "Samourai", "Kamouraska") ; "amitié", correspondance simple.
motif_amour  <- "\\bamour"
motif_amitie <- "amitié"
ids_amour  <- sujets[grepl(motif_amour,  Sujets, ignore.case = TRUE, perl = TRUE), unique(AR_id)]
ids_amitie <- sujets[grepl(motif_amitie, Sujets, ignore.case = TRUE, perl = TRUE), unique(AR_id)]

# Un livre compte pour 1 s'il porte au moins un descripteur correspondant.
# Dénominateur de la figure 2 : le corpus complet.
dt_total <- ids_annee[, .(n_total = uniqueN(AR_id)), by = Annee]

serie_motif <- function(ids_motif, etiquette) {
  dt <- ids_annee[AR_id %in% ids_motif, .(n_motif = uniqueN(AR_id)), by = Annee]
  dt <- dt[data.table(Annee = 2000:2021), on = "Annee"]
  dt[is.na(n_motif), n_motif := 0L]
  dt <- merge(dt, dt_total, by = "Annee")
  dt[, freq_rel := 100 * n_motif / n_total][, motif := etiquette][]
}

# L'étiquette du motif amour reprend la regex, comme dans la légende de la figure.
serie_motifs <- rbind(serie_motif(ids_amour,  "\\bamour"),
                      serie_motif(ids_amitie, "amitié"))

figure_2 <- ggplot(serie_motifs, aes(x = Annee, y = freq_rel,
                                     colour = motif, fill = motif)) +
  geom_col(alpha = 0.2, position = "identity", width = 0.7) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = FALSE, formula = y ~ x,
              linewidth = 1, span = 0.75) +
  scale_colour_manual(values = c("\\bamour" = PAL_AMOUR, "amitié" = PAL_AMITIE)) +
  scale_fill_manual(values   = c("\\bamour" = PAL_AMOUR, "amitié" = PAL_AMITIE)) +
  scale_x_continuous(breaks = seq(2000, 2021, 2)) +
  labs(x = "Année", y = "% des livres du corpus",
       title = "Fréquence relative des motifs", colour = "Motif", fill = "Motif") +
  theme(legend.position = "bottom")

ggsave("figure_2_amour_amitie.png", figure_2, width = 7, height = 4.5, dpi = 300)

# Pente de tendance d'un motif, pour un dénominateur annuel donné. Les deux
# tendances sont calculées sur le corpus complet et sur les seuls livres
# indexés (note 36 de l'article) : la direction du chiasme ne change pas.
ids_indexes  <- sujets[, unique(AR_id)]
dt_total_idx <- ids_annee[AR_id %in% ids_indexes,
                          .(n_total = uniqueN(AR_id)), by = Annee]

pente_motif <- function(ids_motif, denom) {
  dt <- ids_annee[AR_id %in% ids_motif, .(n_motif = uniqueN(AR_id)), by = Annee]
  dt <- dt[data.table(Annee = 2000:2021), on = "Annee"]
  dt[is.na(n_motif), n_motif := 0L]
  dt <- merge(dt, denom, by = "Annee")
  dt[, freq_rel := 100 * n_motif / n_total]
  fit <- lm(freq_rel ~ Annee, data = dt)
  c(pente = unname(coef(fit)[2]), p = summary(fit)$coefficients[2, 4])
}

pentes <- list(
  amour_corpus   = pente_motif(ids_amour,  dt_total),
  amour_indexes  = pente_motif(ids_amour,  dt_total_idx),
  amitie_corpus  = pente_motif(ids_amitie, dt_total),
  amitie_indexes = pente_motif(ids_amitie, dt_total_idx)
)

# Robustesse du chiasme : la forme se retrouve dans chaque zone d'édition et
# dans chaque profil d'éditeur. On calcule, pour chaque sous-population, la
# pente annuelle des deux motifs (dénominateur : livres indexés du groupe).
zone_livre <- merge(livres[, .(AR_id, Lieu_editeur)], cat_geo,
                    by = "Lieu_editeur", all.x = TRUE)
zone_livre[, zone := fcase(
  categorie_geo == "urbain",   "Urbain",
  categorie_geo == "banlieue", "Banlieue",
  categorie_geo == "région",   "Regions"
)]
profil_ed <- livres[, .(n_annees_actif = uniqueN(Annee_source)), by = Editeur]
profil_ed[, profil := fcase(
  n_annees_actif <= 5,  "Ponctuel",
  n_annees_actif <= 10, "Intermittent",
  n_annees_actif >= 11, "Pilier"
)]
profil_livre <- merge(livres[, .(AR_id, Editeur)], profil_ed[, .(Editeur, profil)],
                      by = "Editeur")

pente_groupe <- function(ids_groupe, ids_motif) {
  ids_g <- intersect(ids_groupe, ids_indexes)
  denom <- ids_annee[AR_id %in% ids_g, .(n_total = uniqueN(AR_id)), by = Annee]
  num   <- ids_annee[AR_id %in% intersect(ids_g, ids_motif),
                     .(n_motif = uniqueN(AR_id)), by = Annee]
  dt <- merge(denom, num, by = "Annee", all.x = TRUE)
  dt[is.na(n_motif), n_motif := 0L]
  dt[, freq_rel := 100 * n_motif / n_total]
  unname(coef(lm(freq_rel ~ Annee, data = dt))[2])
}

robustesse_zone <- rbindlist(lapply(c("Urbain", "Banlieue", "Regions"), function(z) {
  ids_z <- zone_livre[zone == z, AR_id]
  data.table(zone = z,
             pente_amour  = pente_groupe(ids_z, ids_amour),
             pente_amitie = pente_groupe(ids_z, ids_amitie))
}))
robustesse_profil <- rbindlist(lapply(c("Pilier", "Intermittent", "Ponctuel"), function(p) {
  ids_p <- profil_livre[profil == p, AR_id]
  data.table(profil = p,
             pente_amour  = pente_groupe(ids_p, ids_amour),
             pente_amitie = pente_groupe(ids_p, ids_amitie))
}))

# Décomposition de la hausse : le sous-genre sentimental est celui dont
# l'investissement du thème amoureux décroit le plus, tandis que l'amitié y
# fait son nid.
evolution_sous_genre <- function(ids_motif, sous_genre_cible) {
  ids_sg <- genres[Sous_genre == sous_genre_cible, unique(AR_id)]
  dt <- ids_annee[AR_id %in% intersect(ids_motif, ids_sg),
                  .(n = uniqueN(AR_id)), by = Annee]
  dt <- dt[data.table(Annee = 2000:2021), on = "Annee"]
  dt[is.na(n), n := 0L]
  c(moy_debut = dt[Annee %in% 2000:2002, mean(n)],
    moy_fin   = dt[Annee %in% 2019:2021, mean(n)])
}
sentimental_amour  <- evolution_sous_genre(ids_amour,  "sentimental")
sentimental_amitie <- evolution_sous_genre(ids_amitie, "sentimental")

# "Littérature de fille" : le sous-genre porteur de la hausse de l'amitié.
# L'article décrit ce croisement sous deux angles, calculés l'un et l'autre
# avec le descripteur "Amitié féminine".
ids_ldf <- genres[Sous_genre == "littérature de filles", unique(AR_id)]
ids_af  <- sujets[Sujets == "Amitié féminine", unique(AR_id)]

croisement <- function(ids_theme, ids_genre, annees) {
  ids_p <- ids_annee[Annee %in% annees, AR_id]
  th    <- intersect(ids_theme, ids_p)
  ge    <- intersect(ids_genre, ids_p)
  inter <- intersect(th, ge)
  c(pct_theme_dans_genre = 100 * length(inter) / length(th),  # part du thème qui relève du genre
    pct_genre_dans_theme = 100 * length(inter) / length(ge))  # part du genre qui porte le thème
}
# Premier angle : part des livres "Amitié féminine" relevant aussi de la
# littérature de fille, au début puis à la fin de la période.
af_dans_ldf_debut <- croisement(ids_af, ids_ldf, 2000:2002)["pct_theme_dans_genre"]
af_dans_ldf_fin   <- croisement(ids_af, ids_ldf, 2019:2021)["pct_theme_dans_genre"]
# Second angle : part de la littérature de fille portant le descripteur
# "Amitié féminine", sur l'ensemble de la période.
ldf_avec_af <- croisement(ids_af, ids_ldf, 2000:2021)["pct_genre_dans_theme"]

# Décomposition de la hausse du motif amitié descripteur par descripteur :
# "Amitié féminine" en porte l'essentiel.
H_global <- (ids_annee[AR_id %in% ids_amitie & Annee %in% 2019:2021, uniqueN(AR_id)] -
             ids_annee[AR_id %in% ids_amitie & Annee %in% 2000:2002, uniqueN(AR_id)]) / 3

moyenne_triennale_descripteur <- function(descripteur, annees) {
  ids_d <- sujets[Sujets == descripteur, unique(AR_id)]
  length(intersect(ids_d, ids_annee[Annee %in% annees, AR_id])) / length(annees)
}
descripteurs_amitie <- sujets[grepl(motif_amitie, Sujets, ignore.case = TRUE, perl = TRUE),
                              unique(Sujets)]
decomposition_amitie <- rbindlist(lapply(descripteurs_amitie, function(d) {
  md <- moyenne_triennale_descripteur(d, 2000:2002)
  mf <- moyenne_triennale_descripteur(d, 2019:2021)
  data.table(descripteur = d, diff = mf - md,
             pct_hausse_globale = 100 * (mf - md) / H_global)
}))
setorder(decomposition_amitie, -diff)

# Trajectoires comparées des deux variantes genrées : l'asymétrie est nette.
serie_descripteur <- function(descripteur) {
  ids_d <- sujets[Sujets == descripteur, unique(AR_id)]
  dt <- ids_annee[AR_id %in% ids_d, .(n = uniqueN(AR_id)), by = Annee]
  dt <- dt[data.table(Annee = 2000:2021), on = "Annee"]
  dt[is.na(n), n := 0L][]
}
tendance_descripteur <- function(descripteur) {
  fit <- lm(n ~ Annee, data = serie_descripteur(descripteur))
  c(pente = unname(coef(fit)[2]), r2 = summary(fit)$r.squared,
    p = summary(fit)$coefficients[2, 4])
}
tendance_amitie_feminine  <- tendance_descripteur("Amitié féminine")
tendance_amitie_masculine <- tendance_descripteur("Amitié masculine")

# "Amitié féminine" hors "littérature de fille" : près de la moitié du
# sous-corpus, publiée majoritairement par des piliers urbains.
ids_af_hors_ldf <- setdiff(ids_af, ids_ldf)
editeurs_af_hors_ldf <- merge(
  livres[AR_id %in% ids_af_hors_ldf, .(AR_id, Editeur)],
  dt_editeurs[, .(Editeur, profil, zone_dom)], by = "Editeur", all.x = TRUE
)[, .(n = uniqueN(AR_id)), by = .(Editeur, profil, zone_dom)][order(-n)]

# Le basculement de Les Éditeurs réunis : la maison domine le sous-corpus
# "Amitié féminine" pris dans son ensemble, mais se trouve reléguée dès qu'on
# en retranche la littérature de fille.
rang_editeurs <- function(ids) {
  dt <- livres[AR_id %in% ids, .(n = uniqueN(AR_id)), by = Editeur][order(-n)]
  dt[, rang := .I][]
}
rang_dans_amitie_feminine <- rang_editeurs(ids_af)[Editeur == "Les Éditeurs réunis"]
rang_dans_af_hors_ldf     <- rang_editeurs(ids_af_hors_ldf)[Editeur == "Les Éditeurs réunis"]

# =============
#     Fin
# =============
