# TODO:

 - mettre à jour vignette
  "For a vignette which complements this introduction, see http://rpubs.com/dcomtois/summarytools_vignette
 - Uniformiser titres de section (mettre variable sous le nom de la fonction,
   et data frame en dessous de tout ça)
 - dans descr, display.labels doit être mis en action dans print.summarytools et non dans descr (déjà
   commencé à travailler là-dessus)
 - réviser la documentation de chaque fonction, notamment les mots-clés.
 - remettre "escape.pipes" comme arguement de dfSummary, ou le récupérer dans print.summarytools
 - problème avec lapply() et freq() - On ne devrait pas voir Frequencies pour chaque variable
 - Textgraph parfois +++ large (ex: dfSummary(tobacco), nb.cigs.per.day)
 - Vérifier les Regex dans parse_args (ok?)
- quand file = "xxxxx.html", d?clencher print et copier le fichier html dont le chemin est retourn?; ajouter un argument "exec" ou "open" ? print.summarytools
- option missing est document?e dans le readme mais pas dans le package. Ajouter un lien vers pander options?
- Comment transformer les fichiers texte.md en texte.html
- Que se passe-t-il quand on essaie de convertir des documents multilignes de md ? html avec pandoc?

