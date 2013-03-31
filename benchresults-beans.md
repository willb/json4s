# Benchresults

## Serialization
Java serialization (ser)          232ms  
Java noop                           0ms  
Java toString (ser)               115ms  

### Jackson with Scala module
Jackson serialization (full)      207ms  
Jackson serialization (ser)        64ms  
Jackson (deser)                   107ms  
Jackson AST (parse)               603ms  
Jackson AST (ser)                  26ms  
  
### Json4s direct AST
json4s-native AST (parse)         808ms  
json4s-jackson AST (parse)        493ms  
json4s-native AST (ser)           212ms  
json4s-jackson AST (ser)           88ms  

### Custom serializer
json4s-native (full)              532ms  
json4s-jackson (full)             540ms  
json4s-native (ser)               177ms  
json4s-jackson (ser)              187ms  
json4s-native (deser)             308ms  
json4s-jackson (deser)            280ms  
json4s-native old pretty          376ms  

### No type hints
json4s-native (full)              803ms  
json4s-jackson (full)             816ms  
json4s-native (ser)               246ms  
json4s-jackson (ser)              292ms  
json4s-native (deser)             471ms  
json4s-jackson (deser)            438ms  
json4s-native old pretty          521ms  

### Short type hints
json4s-native (full)             1207ms  
json4s-jackson (full)            1199ms  
json4s-native (ser)               317ms  
json4s-jackson (ser)              363ms  
json4s-native (deser)             771ms  
json4s-jackson (deser)            729ms  
json4s-native old pretty          687ms  

### Full type hints
json4s-native (full)             1632ms  
json4s-jackson (full)            1580ms  
json4s-native (ser)               540ms  
json4s-jackson (ser)              574ms  
json4s-native (deser)             955ms  
json4s-jackson (deser)            887ms  
json4s-native old pretty          911ms  
