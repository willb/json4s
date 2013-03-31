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
json4s-native AST (parse)         759ms
json4s-jackson AST (parse)        514ms
json4s-native AST (ser)           207ms
json4s-jackson AST (ser)           90ms

### Custom serializer
json4s-native (full)              509ms  
json4s-jackson (full)             539ms  
json4s-macros (full)              276ms  
json4s-native (ser)               163ms  
json4s-jackson (ser)              203ms  
json4s-macros (ser)               167ms  
json4s-native (deser)             301ms  
json4s-jackson (deser)            288ms  
json4s-macros (deser)              84ms  

### No type hints
json4s-native (full)              836ms  
json4s-jackson (full)             895ms  
json4s-macros (full)              293ms  
json4s-native (ser)               254ms  
json4s-jackson (ser)              340ms  
json4s-macros (ser)               168ms  
json4s-native (deser)             486ms  
json4s-jackson (deser)            503ms  
json4s-macros (deser)              83ms  

### Short type hints
json4s-native (full)             1171ms  
json4s-jackson (full)            1257ms  
json4s-macros (full)              303ms  
json4s-native (ser)               313ms  
json4s-jackson (ser)              395ms  
json4s-macros (ser)               173ms  
json4s-native (deser)             771ms  
json4s-jackson (deser)            757ms  
json4s-macros (deser)             108ms  

### Full type hints
json4s-native (full)             1530ms  
json4s-jackson (full)            1633ms  
json4s-macros (full)              285ms  
json4s-native (ser)               553ms  
json4s-jackson (ser)              630ms  
json4s-macros (ser)               167ms  
json4s-native (deser)             899ms  
json4s-jackson (deser)            890ms  
json4s-macros (deser)             112ms  
