# source(file.path(root,"resources/server_tables.R"))
output$admin <- reactive(rvAdmin())
outputOptions(output,"admin",suspendWhenHidden=FALSE)
output$pinEntered <- reactive(is.integer(rvPinEntered()))
outputOptions(output,"pinEntered",suspendWhenHidden=FALSE)
output$authorized <- reactive(is.integer(rvAuthorized()))
outputOptions(output,"authorized",suspendWhenHidden=FALSE)
output$userEmpty <- reactive(!length(exchange$config$user))
outputOptions(output,"userEmpty",suspendWhenHidden=FALSE)
if (staffOnly) { ## output.rebuildNAO
   output$rebuildNAO <- reactive({
      isTRUE(exchange$rebuildNAO)
   })
   outputOptions(output,"rebuildNAO",suspendWhenHidden=FALSE)
}
