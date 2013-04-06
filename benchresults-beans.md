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
json4s-native AST (parse)         807ms  
json4s-jackson AST (parse)        506ms  
json4s-native AST (ser)           199ms  
json4s-jackson AST (ser)           84ms  

### Custom serializer
json4s-native (full)              514ms  
json4s-jackson (full)             551ms  
json4s-native (ser)               171ms  
json4s-jackson (ser)              196ms  
json4s-native (deser)             302ms  
json4s-jackson (deser)            297ms  
json4s-native old pretty          390ms  

### No type hints
json4s-native (full)              726ms  
json4s-jackson (full)             767ms  
json4s-native (ser)               245ms  
json4s-jackson (ser)              306ms  
json4s-native (deser)             394ms  
json4s-jackson (deser)            401ms  
json4s-native old pretty          514ms  
  
### Short type hints
json4s-native (full)             1137ms  
json4s-jackson (full)            1199ms  
json4s-native (ser)               322ms  
json4s-jackson (ser)              397ms  
json4s-native (deser)             693ms  
json4s-jackson (deser)            683ms  
json4s-native old pretty          670ms  

### Full type hints
json4s-native (full)             1429ms  
json4s-jackson (full)            1479ms  
json4s-native (ser)               537ms  
json4s-jackson (ser)              605ms  
json4s-native (deser)             796ms  
json4s-jackson (deser)            782ms  
json4s-native old pretty          897ms  