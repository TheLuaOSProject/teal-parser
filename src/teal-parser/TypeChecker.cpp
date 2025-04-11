#include "TypeChecker.hpp"
#include "Builtins.hpp" // To populate global scope
#include <iostream>     // For debug prints
#include <set>

namespace teal::parser::typechecker
{
    using namespace teal::parser; // Access AST structures easily

    TypeChecker::TypeChecker(ErrorCollector &err)
        : env(err), errors(err)
    {
        builtins::populateGlobalEnvironment(env);
    }

    void TypeChecker::check(const ast::ASTNode *root) {
        if (not root) {
            errors.addError("Internal error: Cannot check null AST root.", 0, 0);
            return;
        }

        // Determine the root node type and start checking
        if (auto *block = dynamic_cast<const ast::Block*>(root)) {
            checkBlock(block, false); // Check top-level block without extra scope
        } else if (auto *stmt = dynamic_cast<const ast::Statement*>(root)) {
             // Allow checking single statements? Usually starts with a block.
              checkStatement(stmt);
        } else if (auto *expr = dynamic_cast<const ast::Expression*>(root)) {
            // Allow checking single expressions? Usually starts with a block.
             checkExpression(expr);
        }
        else {
             errors.addError("Internal error: Unknown AST root node type.", root->line, root->column);
        }
    }

    // --- Type Resolution Implementation ---

    std::shared_ptr<const Type> TypeChecker::resolveTypeNode(const ast::TypeNode *node) {
        if (not node) return env.getUnknownType();

        // Use dynamic_cast to dispatch to specific resolution methods
        if (auto *basic = dynamic_cast<const ast::BasicTypeNode*>(node)) return resolveBasicType(basic);
        if (auto *nominal = dynamic_cast<const ast::NominalTypeNode*>(node)) return resolveNominalType(nominal);
        if (auto *table = dynamic_cast<const ast::TableTypeNode*>(node)) return resolveTableType(table);
        if (auto *func = dynamic_cast<const ast::FunctionTypeNode*>(node)) return resolveFunctionType(func);
        if (auto *onion = dynamic_cast<const ast::UnionTypeNode*>(node)) return resolveUnionType(onion); // 'union' is keyword
        if (auto *record = dynamic_cast<const ast::TypeRecordNode*>(node)) return resolveRecordTypeNode(record);
        if (auto *enumNode = dynamic_cast<const ast::TypeEnumNode*>(node)) return resolveEnumTypeNode(enumNode);
        if (auto *require = dynamic_cast<const ast::RequireTypeNode*>(node)) return resolveRequireType(require);

        errors.addError("Internal error: Unknown TypeNode kind.", *node);
        return env.getUnknownType();
    }

     std::shared_ptr<const Type> TypeChecker::resolveBasicType(const ast::BasicTypeNode *node) {
         const std::string &name = node->name;
         if (name == "any") return env.getAnyType();
         if (name == "nil") return env.getNilType();
         if (name == "boolean") return env.getBooleanType();
         if (name == "integer") return env.getIntegerType();
         if (name == "number") return env.getNumberType();
         if (name == "string") return env.getStringType();
         if (name == "thread") return env.getThreadType();
         // Add other builtins like 'userdata' if needed

         // If not a builtin basic type, try looking up as a Type Alias or other named type
         auto symbol = env.lookupType(name);
         if (symbol and symbol->type) {
            return symbol->type;
         }

         errors.addError("Unknown basic type name: '" + name + "'.", *node);
         return env.getUnknownType();
     }

     std::shared_ptr<const Type> TypeChecker::resolveNominalType(const ast::NominalTypeNode *node) {
          // TODO: Handle generics (type_arguments) later
         if (not node->type_arguments.empty()) {
              errors.addError("Generics are not yet supported.", *node);
             // Continue trying to resolve the base type for now
         }

         auto symbol = env.lookupType(node->name_parts);
         if (symbol) {
            if (not symbol->isDefined) {
                 errors.addError("Type '" + symbol->name + "' is used before its definition is complete.", *node);
                 // Return the undefined symbol's type (likely Unknown) or a specific marker?
                  return symbol->type ? symbol->type : env.getUnknownType();
            }
             return symbol->type;
         } else {
             // Construct the full name for the error message
             std::string fullName;
             for(size_t i=0; i<node->name_parts.size(); ++i) {
                fullName += node->name_parts[i];
                if (i < node->name_parts.size()-1) fullName += ".";
             }
             errors.addError("Unknown type name: '" + fullName + "'.", *node);
             return env.getUnknownType();
         }
     }

      std::shared_ptr<const Type> TypeChecker::resolveTableType(const ast::TableTypeNode *node) {
          if (node->is_map) {
              if (not node->key_type or node->element_types.size() != 1) {
                  errors.addError("Invalid map type definition: requires exactly one key type and one value type.", *node);
                  return env.getUnknownType();
              }
              auto keyType = resolveTypeNode(node->key_type.get());
              auto valueType = resolveTypeNode(node->element_types[0].get());
              if (not keyType or keyType->isNil() or keyType->isAny()) { // Basic check for valid map keys
                  errors.addError("Invalid map key type: '" + (keyType ? keyType->toString() : "null") + "'. Keys cannot be nil or any.", *node);
                  // Use Unknown or default to String? Let's use Unknown.
                  keyType = env.getUnknownType();
              }

              if (keyType == env.getUnknownType() or valueType == env.getUnknownType()) {
                    return env.getUnknownType(); // Propagate error
              }
              return std::make_shared<MapType>(keyType, valueType);
          } else {
              // Array or Tuple
              if (node->element_types.empty()) {
                  errors.addError("Cannot determine type of empty table literal '{}'. Use an explicit type annotation.", *node);
                  // Default to {any}? Or error? Let's default to {any} for now.
                  // return env.getUnknownType();
                  return std::make_shared<ArrayType>(env.getAnyType());
              } else if (node->element_types.size() == 1 and not node->key_type) {
                  // Array: {T}
                  auto elementType = resolveTypeNode(node->element_types[0].get());
                   if (elementType == env.getUnknownType()) return env.getUnknownType();
                  return std::make_shared<ArrayType>(elementType);
              } else if (not node->key_type) {
                  // Tuple: {T1, T2, ...}
                  std::vector<std::shared_ptr<const Type>> elementTypes;
                  bool error = false;
                  elementTypes.reserve(node->element_types.size());
                  for (const auto &elemNode : node->element_types) {
                      auto elemType = resolveTypeNode(elemNode.get());
                      if(elemType == env.getUnknownType()) error = true;
                      elementTypes.push_back(elemType);
                  }
                   if (error) return env.getUnknownType();
                  return std::make_shared<TupleType>(std::move(elementTypes));
              } else {
                   errors.addError("Invalid table type syntax.", *node);
                   return env.getUnknownType();
              }
          }
      }

      std::shared_ptr<const Type> TypeChecker::resolveFunctionType(const ast::FunctionTypeNode *node) {
           // TODO: Handle generics (type_parameters) later
         if (not node->type_parameters.empty()) {
              errors.addError("Generics are not yet supported.", *node);
         }
         return buildFunctionType(*node); // Use helper
      }

       std::shared_ptr<const Type> TypeChecker::resolveUnionType(const ast::UnionTypeNode *node) {
           if (node->options.empty()) {
                errors.addError("Union type must have at least one option.", *node);
                return env.getUnknownType();
           }
           std::vector<std::shared_ptr<const Type>> optionTypes;
           bool error = false;
           optionTypes.reserve(node->options.size());
           for (const auto &optNode : node->options) {
               auto optType = resolveTypeNode(optNode.get());
               if (optType == env.getUnknownType()) error = true;
                // Don't add Unknown to the union, just propagate the error flag
               if (optType != env.getUnknownType()) {
                    optionTypes.push_back(optType);
               }
           }

           if (error) return env.getUnknownType();
           if (optionTypes.empty()) { // All options resolved to Unknown
                return env.getUnknownType();
           }

           // Validate union constraints (from guide)
           int tableLikeCount = 0;
           bool hasString = false;
           bool hasEnum = false;
           std::set<const RecordType*> discriminatedRecords; // Check for `where` clauses if multiple records

            for(const auto &opt : optionTypes) {
                auto realOpt = resolveAliases(opt);
                if (not realOpt) continue;
                if (realOpt->isTableLike()) {
                    tableLikeCount++;
                    if(realOpt->kind == Type::Kind::RECORD) {
                        auto rec = static_cast<const RecordType*>(realOpt.get());
                        if (rec->whereClause) {
                            discriminatedRecords.insert(rec);
                        }
                    }
                }
                if (realOpt->isString()) hasString = true;
                if (realOpt->kind == Type::Kind::ENUM) hasEnum = true;
            }

            if (tableLikeCount > 1 and discriminatedRecords.size() != (size_t)tableLikeCount) {
                 // Allow multiple records ONLY if all have `where` clauses (simplistic check)
                 // The guide implies *all *table types in the union need discrimination if count > 1
                  errors.addError("Union types cannot contain multiple table-like types (array, tuple, map, record, interface) unless they are all records/interfaces discriminated with 'where' clauses.", *node);
                 // Don't return Unknown yet, maybe allow the union but warn? Let's return Unknown for now.
                 return env.getUnknownType();
            }
             if (hasString and hasEnum) {
                 errors.addError("Union types cannot contain both 'string' and an enum type due to runtime ambiguity.", *node);
                 return env.getUnknownType();
             }
             // Check for multiple enums? Guide implies this is also disallowed.
             // Check A | A simplification is handled by UnionType constructor


           if (optionTypes.size() == 1) {
                return optionTypes[0]; // Not really a union
           }

           return std::make_shared<UnionType>(std::move(optionTypes));
       }

        std::shared_ptr<const Type> TypeChecker::resolveRecordTypeNode(const ast::TypeRecordNode *node) {
            // This creates an *anonymous *structural record type, if used directly as a type.
            // Teal focuses on nominal types, so is this common?
            // Usually, records are defined via RecordDeclarationStatement.
            // Let's treat this like defining an anonymous record for now.
             errors.addError("Anonymous record type definitions are not fully supported yet. Define records using 'local record ... end'.", *node);

            // Partial implementation: Create a temporary RecordType
             auto anonRecord = std::make_shared<RecordType>("<anonymous record>"); // Need a mutable type temporarily
             // Need mutable copy to pass to processRecordBody
             auto mutableAnonRecord = std::make_shared<RecordType>(*anonRecord);
             env.pushScope(); // Scope for the record body processing
             processRecordBody(*node->body, mutableAnonRecord.get());
             env.popScope();

             // Return the (now const) processed type
             return mutableAnonRecord;
        }

        std::shared_ptr<const Type> TypeChecker::resolveEnumTypeNode(const ast::TypeEnumNode *node) {
             errors.addError("Anonymous enum type definitions are not supported. Define enums using 'local enum ... end'.", *node);
             // Could potentially create an anonymous EnumType here if needed.
            return env.getUnknownType();
        }

        std::shared_ptr<const Type> TypeChecker::resolveRequireType(const ast::RequireTypeNode *node) {
             // Module system support needed.
             errors.addError("Type checking across modules ('require') is not yet implemented.", *node);
             // For now, return 'any' or 'unknown' for required types.
             return env.getAnyType();
        }


    // --- Expression Type Checking Implementation ---

    std::shared_ptr<const Type> TypeChecker::checkExpression(const ast::Expression *expr) {
     // Public API doesn't usually have a hint, call internal helper without one
          return checkExpressionInternal(expr, nullptr);
     } 

     std::shared_ptr<const Type> TypeChecker::checkExpressionInternal(
          const ast::Expression *expr,
          const std::shared_ptr<const Type>& expectedTypeHint // Receive hint
      ) {
           if (not expr) return env.getUnknownType();
      
          // Dispatch using dynamic_cast
          if (auto *node = dynamic_cast<const ast::NameExpression*>(expr)) return checkNameExpression(node);
          if (auto *node = dynamic_cast<const ast::NumberExpression*>(expr)) return checkNumberExpression(node);
        if (auto *node = dynamic_cast<const ast::StringExpression*>(expr)) return checkStringExpression(node);
        if (auto *node = dynamic_cast<const ast::BooleanExpression*>(expr)) return checkBooleanExpression(node);
        if (auto *node = dynamic_cast<const ast::NilExpression*>(expr)) return checkNilExpression(node);
        if (auto *node = dynamic_cast<const ast::VarargExpression*>(expr)) return checkVarargExpression(node);
        if (auto *node = dynamic_cast<const ast::FunctionCallExpression*>(expr)) return checkFunctionCallExpression(node);
        if (auto *node = dynamic_cast<const ast::IndexExpression*>(expr)) return checkIndexExpression(node);
        if (auto *node = dynamic_cast<const ast::FieldExpression*>(expr)) return checkFieldExpression(node);
        if (auto *node = dynamic_cast<const ast::BinaryOperationExpression*>(expr)) return checkBinaryOperationExpression(node);
        if (auto *node = dynamic_cast<const ast::UnaryOperationExpression*>(expr)) return checkUnaryOperationExpression(node);
        if (auto *node = dynamic_cast<const ast::FunctionDefinitionExpression*>(expr)) return checkFunctionDefinitionExpression(node);
        if (auto *node = dynamic_cast<const ast::CastExpression*>(expr)) return checkCastExpression(node);
        if (auto *node = dynamic_cast<const ast::IsTypeExpression*>(expr)) return checkIsTypeExpression(node);
        if (auto *node = dynamic_cast<const ast::TableConstructorExpression*>(expr)) {
          // ---> Pass the hint down <---
          return checkTableConstructorExpression(node, expectedTypeHint);
          }

        errors.addError("Internal error: Unhandled expression type.", *expr);
        return env.getUnknownType();
    }

    std::shared_ptr<const Type> TypeChecker::checkNameExpression(const ast::NameExpression *expr) {
        auto symbol = env.lookupValue(expr->name);
        if (not symbol) {
            errors.addError("Undefined variable: '" + expr->name + "'.", *expr);
            return env.getUnknownType();
        }
        if (symbol->kind != SymbolKind::VARIABLE and symbol->kind != SymbolKind::FUNCTION /*and symbol->kind != SymbolKind::ENUM_MEMBER*/) {
             errors.addError("Symbol '" + expr->name + "' is not a variable or function.", *expr);
             return env.getUnknownType();
        }
        return symbol->type ? symbol->type : env.getUnknownType();
    }

    std::shared_ptr<const Type> TypeChecker::checkNumberExpression(const ast::NumberExpression *expr) {
        // Check if it contains '.' or 'e'/'E' to distinguish float from integer?
        // Lua's parser handles this, but Teal distinguishes types.
        // Simple approach: if it looks like an integer, return Integer, else Number.
        // More robust: try parsing.
        if (expr->value.find_first_of(".eE") == std::string::npos) {
            // Potentially integer, try parsing (optional, can just return Integer)
             try {
                 [[maybe_unused]] long long val = std::stoll(expr->value);
                 return env.getIntegerType();
             } catch (const std::out_of_range&) {
                  // Too large for long long, treat as number
                  return env.getNumberType();
             } catch (const std::invalid_argument&) {
                  // Invalid format (e.g., hex?), treat as number (Lua handles hex/etc.)
                  return env.getNumberType();
             }
        } else {
            return env.getNumberType();
        }
    }
     std::shared_ptr<const Type> TypeChecker::checkStringExpression(const ast::StringExpression*) {
         return env.getStringType();
     }
     std::shared_ptr<const Type> TypeChecker::checkBooleanExpression(const ast::BooleanExpression*) {
         return env.getBooleanType();
     }
      std::shared_ptr<const Type> TypeChecker::checkNilExpression(const ast::NilExpression*) {
          return env.getNilType();
      }

      std::shared_ptr<const Type> TypeChecker::checkVarargExpression(const ast::VarargExpression *expr) {
          errors.addError("Vararg '...' expressions are not yet supported.", *expr);
          return env.getUnknownType(); // Or a specific VarargType?
      }

     std::shared_ptr<const Type> TypeChecker::checkFunctionCallExpression(const ast::FunctionCallExpression *expr) {
          std::shared_ptr<const Type> baseType = checkExpression(expr->base.get());
          if (baseType == env.getUnknownType()) return env.getUnknownType(); // Error already reported

          auto realBaseType = resolveAliases(baseType);
           if (not realBaseType) return env.getUnknownType();


          std::shared_ptr<const FunctionType> funcType = nullptr;
          std::shared_ptr<const Type> selfType = nullptr; // For method calls

          if (not expr->method_name.empty()) { // Method call: base:method(...)
                if (realBaseType->kind != Type::Kind::RECORD and realBaseType->kind != Type::Kind::INTERFACE) {
                    errors.addError("Cannot call method '" + expr->method_name + "' on non-record/interface type '" + realBaseType->toString() + "'. Use '.' for function fields.", *expr->base);
                    return env.getUnknownType();
                }

                std::optional<FieldInfo> fieldInfo;
                 if(realBaseType->kind == Type::Kind::RECORD) {
                     fieldInfo = static_cast<const RecordType*>(realBaseType.get())->findField(expr->method_name);
                 } else { // Interface
                     fieldInfo = static_cast<const InterfaceType*>(realBaseType.get())->findField(expr->method_name);
                 }

                if (not fieldInfo) {
                     errors.addError("No field or method '" + expr->method_name + "' found in type '" + realBaseType->toString() + "'.", *expr); // Error on the method name token? Need token info.
                    return env.getUnknownType();
                }

                auto fieldType = resolveAliases(fieldInfo->type);
                if (not fieldType or fieldType->kind != Type::Kind::FUNCTION) {
                     errors.addError("Field '" + expr->method_name + "' in type '" + realBaseType->toString() + "' is not a function, cannot call with ':'.", *expr);
                    return env.getUnknownType();
                }

                funcType = std::static_pointer_cast<const FunctionType>(fieldType);
                selfType = realBaseType; // The base object is the implicit 'self'

          } else { // Regular function call: base(...)
                if (realBaseType->kind != Type::Kind::FUNCTION) {
                    // TODO: Check for __call metamethod later
                     errors.addError("Attempt to call a non-function type '" + realBaseType->toString() + "'.", *expr->base);
                    return env.getUnknownType();
                }
                 funcType = std::static_pointer_cast<const FunctionType>(realBaseType);
          }

         // Now check arguments against funcType
          auto returnTypes = checkFunctionArguments(*expr, *funcType, selfType);

          if (returnTypes.empty()) {
              return env.getNilType(); // Function returns void (implicitly nil in expression context)
          } else if (returnTypes.size() == 1) {
              return returnTypes[0]; // Single return value
          } else {
              // Multiple return values - wrap in MultiReturnType for context
              return std::make_shared<MultiReturnType>(std::move(returnTypes));
          }
     }


     std::shared_ptr<const Type> TypeChecker::checkIndexExpression(const ast::IndexExpression *expr) {
         auto tableType = resolveAliases(checkExpression(expr->table.get()));
         auto indexType = resolveAliases(checkExpression(expr->index.get()));

         if (tableType == env.getUnknownType() or indexType == env.getUnknownType()) {
             return env.getUnknownType(); // Error already reported
         }
         if(not tableType or not indexType) return env.getUnknownType();


         if (tableType->kind == Type::Kind::ARRAY) {
             const auto *arrType = static_cast<const ArrayType*>(tableType.get());
             if (not indexType->isNumber()) { // Lua allows floats, Teal might prefer integer
                 errors.addError("Array index must be a number or integer, got '" + indexType->toString() + "'.", *expr->index);
                 return env.getUnknownType();
             }
             return arrType->elementType;
         } else if (tableType->kind == Type::Kind::TUPLE) {
             const auto *tupType = static_cast<const TupleType*>(tableType.get());
             // Check if index is a literal number
             if (auto *numLit = dynamic_cast<const ast::NumberExpression*>(expr->index.get())) {
                  try {
                      long long indexVal = std::stoll(numLit->value);
                      if (indexVal >= 1 and static_cast<size_t>(indexVal) <= tupType->elementTypes.size()) {
                          return tupType->elementTypes[indexVal - 1];
                      } else {
                           errors.addError("Tuple index " + std::to_string(indexVal) + " out of range for type '" + tableType->toString() + "'.", *expr->index);
                           return env.getUnknownType();
                      }
                  } catch (...) { // Invalid integer literal format
                       errors.addError("Invalid integer literal for tuple index: '" + numLit->value + "'.", *expr->index);
                       return env.getUnknownType();
                  }
             } else if (indexType->isNumber()) {
                   // Index is a variable, result is union of all tuple element types
                   errors.addError("Indexing a tuple with a variable number results in a union type, which is restricted. Use literal indices.", *expr->index);
                  // Create the union (handle constraints as in resolveUnionType)
                  // return std::make_shared<UnionType>(tupType->elementTypes);
                   return env.getUnknownType(); // Disallow variable indexing for now per guide strictness
             } else {
                   errors.addError("Tuple index must be an integer literal or number, got '" + indexType->toString() + "'.", *expr->index);
                   return env.getUnknownType();
             }
         } else if (tableType->kind == Type::Kind::MAP) {
              const auto *mapType = static_cast<const MapType*>(tableType.get());
              if (not isAssignable(mapType->keyType, indexType)) {
                   errors.addError("Invalid key type for map: expected '" + mapType->keyType->toString() + "', got '" + indexType->toString() + "'.", *expr->index);
                   return env.getUnknownType();
              }
              return mapType->valueType;
         } else if (tableType->isString()) {
               // String indexing is usually done via string.sub, not direct `[]`?
                errors.addError("Direct indexing `[]` on strings is not standard Lua; use string library functions.", *expr->table);
               return env.getUnknownType();
         } else if (tableType->kind == Type::Kind::RECORD or tableType->kind == Type::Kind::INTERFACE) {
              // Check for structural array part `is {T}`
              std::shared_ptr<const Type> structuralBase = nullptr;
               if (tableType->kind == Type::Kind::RECORD) {
                   structuralBase = static_cast<const RecordType*>(tableType.get())->structuralBase;
               } // Interfaces don't have structural base in this design

               if (structuralBase and structuralBase->kind == Type::Kind::ARRAY) {
                   const auto *arrType = static_cast<const ArrayType*>(structuralBase.get());
                    if (not indexType->isNumber()) {
                         errors.addError("Array part index must be a number or integer, got '" + indexType->toString() + "'.", *expr->index);
                         return env.getUnknownType();
                    }
                    return arrType->elementType;
               } else {
                   // TODO: Check for __index metamethod later
                   errors.addError("Type '" + tableType->toString() + "' does not support numerical indexing `[]` (no array part 'is {...}' declared).", *expr->table);
                   return env.getUnknownType();
               }

         } else {
              errors.addError("Type '" + tableType->toString() + "' cannot be indexed with `[]`.", *expr->table);
              return env.getUnknownType();
         }
     }

      std::shared_ptr<const Type> TypeChecker::checkFieldExpression(const ast::FieldExpression *expr) {
          auto objectType = resolveAliases(checkExpression(expr->object.get()));
           if (objectType == env.getUnknownType()) return env.getUnknownType();
           if (not objectType) return env.getUnknownType();

          if (objectType->kind != Type::Kind::RECORD and objectType->kind != Type::Kind::INTERFACE) {
                // TODO: Check for __index metamethod later? (Usually for string keys)
                 errors.addError("Attempt to index non-record/interface type '" + objectType->toString() + "' with dot operator '.'.", *expr->object);
                return env.getUnknownType();
          }

          std::optional<FieldInfo> fieldInfo;
          if(objectType->kind == Type::Kind::RECORD) {
                fieldInfo = static_cast<const RecordType*>(objectType.get())->findField(expr->field);
          } else { // Interface
                fieldInfo = static_cast<const InterfaceType*>(objectType.get())->findField(expr->field);
          }

           if (not fieldInfo) {
                errors.addError("No field '" + expr->field + "' found in type '" + objectType->toString() + "'.", *expr); // Error on the field token? Need token info.
               return env.getUnknownType();
           }

           return fieldInfo->type;
      }


      std::shared_ptr<const Type> TypeChecker::checkBinaryOperationExpression(const ast::BinaryOperationExpression *expr) {
          auto leftType = resolveAliases(checkExpression(expr->left.get()));
          auto rightType = resolveAliases(checkExpression(expr->right.get()));

          if (leftType == env.getUnknownType() or rightType == env.getUnknownType()) {
              return env.getUnknownType();
          }
           if (not leftType or not rightType) return env.getUnknownType();

           TokenType op = expr->operation;

           // Arithmetic (+, -, *, /, //, %, ^)
           if (op == TokenType::ADD or op == TokenType::SUB or op == TokenType::MUL or op == TokenType::DIV or op == TokenType::FLOOR_DIV or op == TokenType::MOD or op == TokenType::BIT_XOR)
           {
               if (not leftType->isNumber() or not rightType->isNumber()) {
                   errors.addError("Arithmetic operator '" + Token::type_to_string(op) + "' requires number operands, got '" + leftType->toString() + "' and '" + rightType->toString() + "'.", *expr);
                   return env.getUnknownType();
               }
               // Result type: integer only if both are integer and operation preserves integer (e.g., not /)
               if (op != TokenType::DIV and leftType->isInteger() and rightType->isInteger()) {
                   return env.getIntegerType();
               } else {
                    return env.getNumberType();
               }
           }
           // Comparison (<, >, <=, >=)
           else if (op == TokenType::LESS or op == TokenType::GREATER or op == TokenType::LESS_EQ or op == TokenType::GREATER_EQ)
           {
                // Allow number op number, string op string
                if (!((leftType->isNumber() and rightType->isNumber()) or (leftType->isString() and rightType->isString()))) {
                    errors.addError("Comparison operator '" + Token::type_to_string(op) + "' requires two numbers or two strings, got '" + leftType->toString() + "' and '" + rightType->toString() + "'.", *expr);
                    return env.getUnknownType();
                }
                return env.getBooleanType();
           }
           // Equality (==, ~=)
            else if (op == TokenType::EQUALS or op == TokenType::NOT_EQ)
            {
                // Allow comparison between compatible types, or anything with nil.
                // What are compatible types? Subtypes? Overlapping unions?
                // Simple: allow if isAssignable(left, right) or isAssignable(right, left) or one is nil.
                bool canCompare = leftType->isNil() or rightType->isNil() or isAssignable(leftType, rightType) or isAssignable(rightType, leftType);
                 // More permissive: allow comparison if types *could *be equal (e.g. number == integer)
                 // Need a better compatibility check here. Let's use the simple assignable check for now.
                 if (not canCompare and not areEqual(leftType, rightType)) { // Check if they might be equal even if not assignable
                    // Check if they are both numbers
                    if(!(leftType->isNumber() and rightType->isNumber())) {
                        // Check primitive vs table etc.
                        if (leftType->isPrimitive() != rightType->isPrimitive() and not leftType->isNil() and not rightType->isNil()) {
                            canCompare = false; // Cannot compare primitive and table (unless nil)
                        } else {
                            // Allow comparison between potentially different types for now, Lua allows this
                            // But issue a warning?
                            // Let's allow it but this could be stricter.
                            canCompare = true;
                        }
                    } else {
                        canCompare = true; // Allow number == integer comparison
                    }

                 }


                if (not canCompare) {
                    errors.addError("Cannot compare types '" + leftType->toString() + "' and '" + rightType->toString() + "' for equality (" + Token::type_to_string(op) + ").", *expr);
                   // Return unknown or boolean? Let's return boolean but report error.
                   // return env.getUnknownType();
                }
                return env.getBooleanType();
            }
           // Logical (and, or) - Complex Lua semantics
            else if (op == TokenType::AND or op == TokenType::OR)
            {
                // Type is complex: depends on truthiness of left operand.
                // `a and b`: if `a` is statically known false/nil -> type is type(a)
                // `a or b`: if `a` is statically known true/non-nil -> type is type(a)
                // Otherwise, result is `Union(type(a), type(b))` potentially simplified.

                // Simple approach: return a union.
                auto unionType = std::make_shared<UnionType>(std::vector<std::shared_ptr<const Type>>{leftType, rightType});
                // Return the simplified union type (constructor handles simplification)
                if (unionType->options.size() == 1) return unionType->options[0];
                else return unionType;
            }
            // Concatenation (..)
            else if (op == TokenType::CONCAT)
            {
                 if (!(leftType->isString() or leftType->isNumber()) or !(rightType->isString() or rightType->isNumber())) {
                      errors.addError("Concatenation operator '..' requires string or number operands, got '" + leftType->toString() + "' and '" + rightType->toString() + "'.", *expr);
                      return env.getUnknownType();
                 }
                 return env.getStringType();
            }
           // Bitwise (&, |, <<, >>) - Assume integer operands for now
            else if (op == TokenType::BIT_AND or op == TokenType::BIT_OR or op == TokenType::SHIFT_L or op == TokenType::SHIFT_R)
            {
                if (not leftType->isInteger() or not rightType->isInteger()) {
                     errors.addError("Bitwise operator '" + Token::type_to_string(op) + "' requires integer operands, got '" + leftType->toString() + "' and '" + rightType->toString() + "'.", *expr);
                     return env.getUnknownType();
                }
                 return env.getIntegerType();
            }
           else {
                errors.addError("Internal error: Unhandled binary operator '" + Token::type_to_string(op) + "'.", *expr);
                return env.getUnknownType();
           }
      }

       std::shared_ptr<const Type> TypeChecker::checkUnaryOperationExpression(const ast::UnaryOperationExpression *expr) {
           auto operandType = resolveAliases(checkExpression(expr->operand.get()));
           if (operandType == env.getUnknownType()) return env.getUnknownType();
            if (not operandType) return env.getUnknownType();

            TokenType op = expr->operation;

           if (op == TokenType::SUB) { // Unary negation
               if (not operandType->isNumber()) {
                    errors.addError("Unary operator '-' requires a number operand, got '" + operandType->toString() + "'.", *expr);
                    return env.getUnknownType();
               }
                // Result type matches operand type (integer or number)
                return operandType;
           } else if (op == TokenType::NOT) { // Logical not
                // 'not' works on any type in Lua
               return env.getBooleanType();
           } else if (op == TokenType::LENGTH) { // Length operator '#'
               if (operandType->isString() or operandType->kind == Type::Kind::ARRAY or operandType->kind == Type::Kind::TUPLE) {
                   return env.getIntegerType();
               } else if(operandType->kind == Type::Kind::RECORD) {
                   // Check for array part 'is {T}'
                    auto structuralBase = static_cast<const RecordType*>(operandType.get())->structuralBase;
                    if(structuralBase and structuralBase->kind == Type::Kind::ARRAY) {
                        return env.getIntegerType();
                    } else {
                         errors.addError("Length operator '#' cannot be used on record type '" + operandType->toString() + "' without an array part 'is {...}'.", *expr);
                         return env.getUnknownType();
                    }
               }
                // TODO: Check __len metamethod later
                else {
                     errors.addError("Length operator '#' requires a string or table (array/tuple/record with array part), got '" + operandType->toString() + "'.", *expr);
                    return env.getUnknownType();
               }
           }
           //I fucking forgot to lex and parse this i am an idiot
        //    else if (op == TokenType::TILDE) { // Bitwise not
        //        if (not operandType->isInteger()) {
        //             errors.addError("Bitwise not operator '~' requires an integer operand, got '" + operandType->toString() + "'.", *expr);
        //             return env.getUnknownType();
        //        }
        //        return env.getIntegerType();
        //    }
           else {
               errors.addError("Internal error: Unhandled unary operator '" + Token::type_to_string(op) + "'.", *expr);
               return env.getUnknownType();
           }
       }

        std::shared_ptr<const Type> TypeChecker::checkFunctionDefinitionExpression(const ast::FunctionDefinitionExpression *expr) {
            // Create the function type
            auto funcType = buildFunctionType(*expr->body);
            if (funcType == env.getUnknownType()) {
                 return env.getUnknownType();
            }

             // Check the function body in a new scope
            env.pushScope();

            // Define parameters in the function scope
            bool paramError = false;
            for(const auto &param : funcType->parameters) {
                 if (param.name) {
                      if (not env.defineValue(*param.name, param.type, true, expr)) { // Parameters are mutable locally
                         paramError = true; // Error defining param
                      }
                 } else {
                     // Parameter without name in definition? Should be caught by parser?
                 }
            }
            // TODO: Define '...' vararg symbol if needed

            if (not paramError) {
                 // Push function context for return type checking
                 env.enterFunction({funcType->returnTypes});
                 checkBlock(expr->body->body.get(), false); // Don't create another scope, already pushed one
                 env.exitFunction();
            }

            env.popScope(); // Pop function scope

            return funcType;
        }

         std::shared_ptr<const Type> TypeChecker::checkCastExpression(const ast::CastExpression *expr) {
             // Check the expression being casted, but ignore its type for the result.
             [[maybe_unused]] auto exprType = checkExpression(expr->expression.get());

             // Resolve the target types
             std::vector<std::shared_ptr<const Type>> targetTypes;
             bool error = false;
             targetTypes.reserve(expr->target_types.size());
             for (const auto &typeNode : expr->target_types) {
                 auto target = resolveTypeNode(typeNode.get());
                 if (target == env.getUnknownType()) error = true;
                 targetTypes.push_back(target);
             }

             if (error) return env.getUnknownType();

             // Teal spec: `as` trusts the programmer. No type checking between exprType and targetType.
             // Return the target type(s).
             if (targetTypes.empty()) {
                  errors.addError("Cast expression 'as' requires at least one target type.", *expr);
                  return env.getUnknownType();
             } else if (targetTypes.size() == 1) {
                 return targetTypes[0];
             } else {
                 // Multiple types specified (e.g., `foo() as (string, number)`)
                 return std::make_shared<MultiReturnType>(std::move(targetTypes));
             }
         }

        std::shared_ptr<const Type> TypeChecker::checkIsTypeExpression(const ast::IsTypeExpression *expr) {
            auto exprType = resolveAliases(checkExpression(expr->expression.get()));
            auto targetType = resolveAliases(resolveTypeNode(expr->type.get()));

            if (exprType == env.getUnknownType() or targetType == env.getUnknownType()) {
                 return env.getUnknownType(); // Error already reported
            }
             if (not exprType or not targetType) return env.getUnknownType();

             // Check if the 'is' check is potentially valid.
             // The check is valid if exprType could possibly be targetType.
             // This means exprType must be 'any', a union containing targetType,
             // or targetType itself, or a supertype of targetType.
             // Example: `x is number` is valid if x is `any`, `number`, `integer`, `number | string`.
             // Example: `x is string` is invalid if x is `number`.

             // Need an 'intersects' check.
             bool possible = false;
             if (exprType->isAny() or targetType->isAny()) {
                 possible = true;
             } else if (areEqual(exprType, targetType) or isSubtype(exprType, targetType) or isSubtype(targetType, exprType)) {
                  possible = true; // Direct match or subtype/supertype relationship
             } else if (exprType->kind == Type::Kind::UNION) {
                 // Check if targetType is one of the options or compatible with one
                  const auto *unionExpr = static_cast<const UnionType*>(exprType.get());
                  for(const auto &opt : unionExpr->options) {
                      if (areEqual(opt, targetType) or isSubtype(opt, targetType) or isSubtype(targetType, opt)) {
                          possible = true;
                          break;
                      }
                  }
             }
              // Add more sophisticated intersection checks if needed (e.g., between two unions)


             if (not possible) {
                  errors.addError("Type check 'is " + targetType->toString() + "' will always be false for expression of type '" + exprType->toString() + "'.", *expr);
                  // Continue, result is still boolean, but report likely logic error.
             }

              // Check runtime limitations on 'is' (from guide)
             if (targetType->kind == Type::Kind::UNION) {
                 errors.addError("Cannot use 'is' operator with a union type ('" + targetType->toString() + "') directly on the right side.", *expr->type);
                 return env.getUnknownType();
             }
             if (targetType->kind == Type::Kind::ARRAY or targetType->kind == Type::Kind::TUPLE or targetType->kind == Type::Kind::MAP) {
                  // Guide seems to imply 'is' works primarily with primitives, records, interfaces, enums?
                  // Need clarification on `is {number}` etc. Assume allowed for now.
             }


             // TODO: Implement type narrowing based on 'is' in control flow (IfStatement)


             return env.getBooleanType(); // Result of 'is' is always boolean
        }

        std::shared_ptr<const Type> TypeChecker::checkTableConstructorExpression(
          const ast::TableConstructorExpression *expr,
          const std::shared_ptr<const Type>& expectedTypeHint // receives an expected type hint
      ) {
          // --- START: Context-Aware Record Checking ---
          auto realExpectedType = resolveAliases(expectedTypeHint);
          if (realExpectedType and realExpectedType->kind == Type::Kind::RECORD) {
              const auto *expectedRecordType = static_cast<const RecordType*>(realExpectedType.get());
              std::set<std::string> providedKeys;
              bool validationOk = true;
      
              for (const auto &fieldVariant : expr->fields) {
                  // Process only key-value pairs for records.
                  if (std::holds_alternative<ast::TableConstructorExpression::KeyValuePair>(fieldVariant)) {
                      const auto &kvPair = std::get<ast::TableConstructorExpression::KeyValuePair>(fieldVariant);
                      std::string fieldName;
                      
                      // Only identifier keys are allowed.
                      if (std::holds_alternative<std::string>(kvPair.key)) {
                          fieldName = std::get<std::string>(kvPair.key);
                      } else {
                          const ast::Expression *keyNode = std::get<std::unique_ptr<ast::Expression>>(kvPair.key).get();
                          errors.addError(
                              "Record initializers only support identifier keys (e.g., { x = 1 }), not computed keys `[...]`.",
                              keyNode ? *keyNode : *expr
                          );
                          validationOk = false;
                          continue;
                      }
      
                      // Check for duplicate fields.
                      if (not providedKeys.insert(fieldName).second) {
                          errors.addError("Duplicate field '" + fieldName + "' in record initializer.", *kvPair.value);
                          validationOk = false;
                      }
      
                      // Verify the field exists in the record definition.
                      auto expectedFieldInfoOpt = expectedRecordType->findField(fieldName);
                      if (not expectedFieldInfoOpt) {
                          errors.addError(
                              "Field '" + fieldName + "' does not exist in record type '" +
                              expectedRecordType->toString() + "'.",
                              *kvPair.value
                          );
                          validationOk = false;
                      } else {
                          // Check the value type against the expected field type.
                          auto valueType = checkExpressionInternal(kvPair.value.get(), expectedFieldInfoOpt->type);
                          if (valueType != env.getUnknownType()) {
                              checkAssignmentCompat(expectedFieldInfoOpt->type, valueType, *kvPair.value);
                              if (not isAssignable(expectedFieldInfoOpt->type, valueType)) {
                                  validationOk = false;
                              }
                          } else {
                              validationOk = false;
                          }
                      }
                      
                      // Validate explicit type annotation on the field, if present.
                      if (kvPair.type) {
                          auto declaredFieldType = resolveTypeNode(kvPair.type.get());
                          if (declaredFieldType != env.getUnknownType() and
                              expectedFieldInfoOpt and not areEqual(declaredFieldType, expectedFieldInfoOpt->type)) {
                              errors.addError(
                                  "Type annotation ':" + declaredFieldType->toString() + "' on field '" + fieldName +
                                  "' conflicts with record's declared type ':" +
                                  expectedFieldInfoOpt->type->toString() + "'.",
                                  *kvPair.type
                              );
                              validationOk = false;
                          }
                      }
                  } else {
                      // Array–style element is not allowed in record initializers.
                      const ast::Expression *valueNode = std::get<std::unique_ptr<ast::Expression>>(fieldVariant).get();
                      errors.addError(
                          "Unexpected array-style element found when initializing record type '" +
                          expectedRecordType->toString() +
                          "'. Use '{ field = value }' syntax.",
                          valueNode ? *valueNode : *expr
                      );
                      validationOk = false;
                  }
              } // end for fields
      
              if (validationOk) {
                  return realExpectedType; // The record literal matches the expected record type.
              } else {
                  return env.getUnknownType();
              }
          }
          // --- END: Context-Aware Record Checking ---
      
          // --- START: Fallback to Standard Table Inference ---
          if (expr->fields.empty()) {
              // For an empty table literal, we default to Array<Any>.
              return std::make_shared<ArrayType>(env.getAnyType());
          }
      
          // Variables for tracking the nature of the table literal.
          bool isPotentialArray = true;
          bool isPotentialTuple = true;
          bool isPotentialMap = true;
          long long expectedArrayIndex = 1;
          std::shared_ptr<const Type> arrayElementType = nullptr;
          std::vector<std::shared_ptr<const Type>> tupleElementTypes;
          std::shared_ptr<const Type> mapKeyType = nullptr;
          std::shared_ptr<const Type> mapValueType = nullptr;
          std::vector<std::shared_ptr<const Type>> mapValueUnionOptions;
      
          // Iterate over each table field to determine if it can be an array, tuple, or map.
          for (const auto &fieldVariant : expr->fields) {
              std::shared_ptr<const Type> valueType = env.getUnknownType();
              std::optional<std::shared_ptr<const Type>> keyTypeOpt = std::nullopt;
              std::optional<long long> arrayIndexOpt = std::nullopt;
      
              if (std::holds_alternative<std::unique_ptr<ast::Expression>>(fieldVariant)) {
                  // Array/Tuple style: { value1, value2, ... }
                  const auto &valueExpr = std::get<std::unique_ptr<ast::Expression>>(fieldVariant);
                  valueType = checkExpressionInternal(valueExpr.get(), nullptr); // no expected hint
                  arrayIndexOpt = expectedArrayIndex;
              } else {
                  // Key-value pair style: { key = value } or { [keyExpr] = value }
                  const auto &kvPair = std::get<ast::TableConstructorExpression::KeyValuePair>(fieldVariant);
                  valueType = checkExpressionInternal(kvPair.value.get(), nullptr); // no expected hint
      
                  // If there is a type annotation (key: type = value) then check it.
                  if (kvPair.type) {
                      auto declaredType = resolveTypeNode(kvPair.type.get());
                      if (declaredType != env.getUnknownType()) {
                          checkAssignmentCompat(declaredType, valueType, *kvPair.value);
                      } else {
                          valueType = env.getUnknownType();
                      }
                  }
      
                  // Determine key type: For identifier keys, use String; for computed keys, evaluate the expression.
                  if (std::holds_alternative<std::string>(kvPair.key)) {
                      keyTypeOpt = env.getStringType();
                  } else {
                      const auto &keyExpr = std::get<std::unique_ptr<ast::Expression>>(kvPair.key);
                      keyTypeOpt = checkExpressionInternal(keyExpr.get(), nullptr);
                      // If the key expression is a literal number, use it as a potential array index.
                      if (auto *numLit = dynamic_cast<const ast::NumberExpression*>(keyExpr.get())) {
                          try {
                              long long indexVal = std::stoll(numLit->value);
                              if (indexVal > 0) {
                                  arrayIndexOpt = indexVal;
                              }
                          } catch (...) {
                              // Ignore if not parseable as an integer.
                          }
                      }
                  }
              }
              if (valueType == env.getUnknownType()) {
                  return env.getUnknownType();
              }
      
              // --- Update potential table type flags ---
      
              // Array: consecutive integer keys with a uniform element type.
              if (isPotentialArray) {
                  if (not arrayIndexOpt.has_value() or arrayIndexOpt.value() != expectedArrayIndex) {
                      isPotentialArray = false;
                  } else {
                      if (not arrayElementType) {
                          arrayElementType = valueType;
                      } else if (not areEqual(arrayElementType, valueType)) {
                          isPotentialArray = false;
                      }
                  }
              }
      
              // Tuple: consecutive integer keys but element types may vary.
              if (isPotentialTuple) {
                  if (not arrayIndexOpt.has_value() or arrayIndexOpt.value() != expectedArrayIndex) {
                      isPotentialTuple = false;
                  } else {
                      tupleElementTypes.push_back(valueType);
                  }
              }
      
              // Map: requires consistent key type and value type (or a union of differing value types).
              if (isPotentialMap) {
                  if (not keyTypeOpt.has_value()) {
                      keyTypeOpt = env.getIntegerType();
                  }
                  if (not mapKeyType) {
                      mapKeyType = keyTypeOpt.value();
                      mapValueType = valueType;
                      mapValueUnionOptions.push_back(valueType);
                  } else {
                      if (not areEqual(mapKeyType, keyTypeOpt.value())) {
                          isPotentialMap = false;
                      } else if (mapValueType and not areEqual(mapValueType, valueType)) {
                          // Mismatch: switch to a union type for values.
                          mapValueUnionOptions.push_back(valueType);
                          mapValueType = nullptr;
                      } else if (not mapValueType) {
                          // Already using union type; add the new type option.
                          mapValueUnionOptions.push_back(valueType);
                      }
                  }
              }
              expectedArrayIndex++; // Prepare for the next potential array or tuple element.
          } // end for loop
      
          // --- Determine final inferred type based on flags ---
          if (isPotentialArray and arrayElementType) {
              return std::make_shared<ArrayType>(arrayElementType);
          }
          if (isPotentialTuple and not tupleElementTypes.empty()) {
              return std::make_shared<TupleType>(std::move(tupleElementTypes));
          }
          if (isPotentialMap and mapKeyType) {
              std::shared_ptr<const Type> finalMapValueType;
              if (mapValueType) {
                  finalMapValueType = mapValueType;
              } else {
                  if (mapValueUnionOptions.size() == 1) {
                      finalMapValueType = mapValueUnionOptions[0];
                  } else {
                      finalMapValueType = std::make_shared<UnionType>(std::move(mapValueUnionOptions));
                  }
              }
              if (not mapKeyType or mapKeyType->isNil() or mapKeyType->isAny()) {
                  errors.addError(
                      "Inferred map key type '" + (mapKeyType ? mapKeyType->toString() : "null") +
                      "' is invalid. Keys cannot be nil or any.",
                      *expr
                  );
                  return env.getUnknownType();
              }
              return std::make_shared<MapType>(mapKeyType, finalMapValueType);
          }
      
          errors.addError(
              "Could not infer a consistent type (array, tuple, or map) for table literal. Use a type annotation or cast.",
              *expr
          );
          return env.getUnknownType();
      } // end checkTableConstructorExpression
    // --- Statement Type Checking Implementation ---

    void TypeChecker::checkStatement(const ast::Statement *stmt) {
        if (not stmt) return;

        // Dispatch using dynamic_cast
        if (auto *node = dynamic_cast<const ast::ReturnStatement*>(stmt)) return checkReturnStatement(node);
        if (auto *node = dynamic_cast<const ast::BreakStatement*>(stmt)) return checkBreakStatement(node);
        if (auto *node = dynamic_cast<const ast::GotoStatement*>(stmt)) return checkGotoStatement(node);
        if (auto *node = dynamic_cast<const ast::LabelStatement*>(stmt)) return checkLabelStatement(node);
        if (auto *node = dynamic_cast<const ast::DoStatement*>(stmt)) return checkDoStatement(node);
        if (auto *node = dynamic_cast<const ast::IfStatement*>(stmt)) return checkIfStatement(node);
        if (auto *node = dynamic_cast<const ast::WhileStatement*>(stmt)) return checkWhileStatement(node);
        if (auto *node = dynamic_cast<const ast::RepeatStatement*>(stmt)) return checkRepeatStatement(node);
        if (auto *node = dynamic_cast<const ast::ForNumericStatement*>(stmt)) return checkForNumericStatement(node);
        if (auto *node = dynamic_cast<const ast::ForInStatement*>(stmt)) return checkForInStatement(node);
        // Note: Declarations might need non-const pointers if they modify the environment's type definitions (e.g., forward -> full)
        if (auto *node = dynamic_cast<ast::FunctionDeclarationStatement*>(const_cast<ast::Statement*>(stmt))) return checkFunctionDeclarationStatement(node);
        if (auto *node = dynamic_cast<const ast::VariableDeclarationStatement*>(stmt)) return checkVariableDeclarationStatement(node);
        if (auto *node = dynamic_cast<ast::RecordDeclarationStatement*>(const_cast<ast::Statement*>(stmt))) return checkRecordDeclarationStatement(node);
        if (auto *node = dynamic_cast<const ast::EnumDeclarationStatement*>(stmt)) return checkEnumDeclarationStatement(node);
        if (auto *node = dynamic_cast<const ast::TypeAliasStatement*>(stmt)) return checkTypeAliasStatement(node);
        if (auto *node = dynamic_cast<const ast::AssignmentStatement*>(stmt)) return checkAssignmentStatement(node);
        if (auto *node = dynamic_cast<const ast::CallStatement*>(stmt)) return checkCallStatement(node);

         errors.addError("Internal error: Unhandled statement type.", *stmt);
    }

     void TypeChecker::checkBlock(const ast::Block *block, bool createNewScope) {
         if (not block) return;
         if (createNewScope) env.pushScope();

         for (const auto &stmt : block->statements) {
             checkStatement(stmt.get());
         }

         if (createNewScope) env.popScope();
     }

    void TypeChecker::checkReturnStatement(const ast::ReturnStatement *stmt) {
        auto funcCtxOpt = env.getCurrentFunctionContext();
        if (not funcCtxOpt) {
            errors.addError("Return statement found outside of a function.", *stmt);
            return;
        }
        const auto &expectedTypes = funcCtxOpt->expectedReturnTypes;

        std::vector<std::shared_ptr<const Type>> actualTypes;
        actualTypes.reserve(stmt->values.size());
        for (size_t i = 0; i < stmt->values.size(); ++i) {
            // ---> Pass expected return type as hint <---
            std::shared_ptr<const Type> hint = (i < expectedTypes.size()) ? expectedTypes[i] : nullptr;
             actualTypes.push_back(checkExpressionInternal(stmt->values[i].get(), hint));
        }

        // Handle potential multi-return from the last expression
        if (not actualTypes.empty() and actualTypes.back()->kind == Type::Kind::MULTI_RETURN) {
            auto multiReturn = std::static_pointer_cast<const MultiReturnType>(actualTypes.back());
            actualTypes.pop_back();
            actualTypes.insert(actualTypes.end(), multiReturn->types.begin(), multiReturn->types.end());
        }


        // Check number of return values
        // TODO: Handle varargs returns later
        if (actualTypes.size() != expectedTypes.size()) {
            errors.addError("Incorrect number of return values: expected " + std::to_string(expectedTypes.size()) + ", got " + std::to_string(actualTypes.size()) + ".", *stmt);
            // Continue checking types for matches that do exist
        }

        // Check types of return values
        size_t checkCount = std::min(actualTypes.size(), expectedTypes.size());
        for (size_t i = 0; i < checkCount; ++i) {
            checkAssignmentCompat(expectedTypes[i], actualTypes[i], *stmt); // Use node of the return statement for error pos
             // Could try to use the specific expression node: *stmt->values[i] if available
        }
    }

     void TypeChecker::checkBreakStatement(const ast::BreakStatement*) { /* No type checking needed */ }
     void TypeChecker::checkGotoStatement(const ast::GotoStatement*) { /* No type checking needed */ }
     void TypeChecker::checkLabelStatement(const ast::LabelStatement*) { /* No type checking needed */ }

     void TypeChecker::checkDoStatement(const ast::DoStatement *stmt) {
         checkBlock(stmt->body.get()); // Creates its own scope
     }

    void TypeChecker::checkIfStatement(const ast::IfStatement *stmt) {
         // TODO: Implement type narrowing based on `is` checks in conditions.
         // This requires passing flow-sensitive type information into the blocks.

        // bool hasElse = stmt->else_block != nullptr;
        // bool firstBranch = true;

        for(const auto &branch : stmt->if_branches) {
            auto condType = checkExpression(branch.condition.get());
             // Condition should evaluate to boolean-like (anything in Lua, stricter in Teal?)
             // Let's allow any type for condition, like Lua. No error here.

             // TODO: If condType resulted from `x is T`, narrow type of x inside block.

            env.pushScope();
            checkBlock(branch.block.get(), false); // Check block in new scope
            env.popScope();

            // firstBranch = false;
        }

        if (stmt->else_block) {
             env.pushScope();
            checkBlock(stmt->else_block.get(), false);
             env.popScope();
        }
    }

     void TypeChecker::checkWhileStatement(const ast::WhileStatement *stmt) {
          auto condType = checkExpression(stmt->condition.get());
          // Allow any type for condition, like Lua.

          env.pushScope(); // Scope for the loop body
          checkBlock(stmt->body.get(), false);
          env.popScope();
     }

     void TypeChecker::checkRepeatStatement(const ast::RepeatStatement *stmt) {
         env.pushScope(); // Scope for the loop body
         checkBlock(stmt->body.get(), false);
         env.popScope(); // Body scope ends before condition is checked

         auto condType = checkExpression(stmt->condition.get());
         // Allow any type for condition, like Lua.
     }

     void TypeChecker::checkForNumericStatement(const ast::ForNumericStatement *stmt) {
          auto startType = checkExpression(stmt->expressions.start.get());
          auto endType = checkExpression(stmt->expressions.end.get());
          auto stepType = stmt->expressions.step ? checkExpression(stmt->expressions.step.get()) : env.getIntegerType(); // Default step is 1 (integer)

          if (not startType->isNumber() or not endType->isNumber() or not stepType->isNumber()) {
               errors.addError("For loop limits and step must be numbers.", *stmt);
               // Don't proceed to check body if limits are wrong type
               return;
          }

          env.pushScope(); // Scope for loop variable and body

           // Define loop variable (always number in standard Lua for)
          if (not env.defineValue(stmt->variable_name, env.getNumberType(), false, stmt)) { // Loop variable is implicitly const? No, can be reassigned inside? Lua docs say no. Let's assume const-like.
             // Error defining variable
          } else {
            checkBlock(stmt->body.get(), false); // Check body in the loop scope
          }

          env.popScope();
     }

      void TypeChecker::checkForInStatement(const ast::ForInStatement *stmt) {
           // This is complex: depends on the iterator protocol.
           // `for <names...> in <exprs...>`
           // Lua evaluates exprs, expects iterator function, state, initial value.
           // Calls iterator(state, initial) -> new_vars...
           // Need type info for standard iterators like ipairs, pairs.

           std::vector<std::shared_ptr<const Type>> iterExprTypes;
           iterExprTypes.reserve(stmt->exprs.size());
           for (const auto &expr : stmt->exprs) {
               iterExprTypes.push_back(checkExpression(expr.get()));
           }

           if (iterExprTypes.empty()) {
                errors.addError("For..in loop requires at least one expression (iterator source).", *stmt);
                return;
           }

           // --- Handle common patterns: ipairs, pairs ---
            std::vector<std::shared_ptr<const Type>> loopVarTypes;
            bool handled = false;

           // Check for `ipairs(table)`
            if (stmt->exprs.size() == 1 and stmt->names.size() <= 2) {
                 if(auto *call = dynamic_cast<const ast::FunctionCallExpression*>(stmt->exprs[0].get())) {
                     if(auto *name = dynamic_cast<const ast::NameExpression*>(call->base.get())) {
                          if (name->name == "ipairs" and call->arguments.size() == 1) {
                              auto tableArgType = resolveAliases(checkExpression(call->arguments[0].get()));
                              if (tableArgType and (tableArgType->kind == Type::Kind::ARRAY or tableArgType->kind == Type::Kind::TUPLE)) {
                                  loopVarTypes.push_back(env.getIntegerType()); // Index 'i'
                                   if (tableArgType->kind == Type::Kind::ARRAY) {
                                       loopVarTypes.push_back(static_cast<const ArrayType*>(tableArgType.get())->elementType);
                                   } else { // Tuple
                                        // Variable ipairs over tuple yields union of element types? Risky.
                                        // Let's return Union for tuple value for now.
                                         const auto *tupType = static_cast<const TupleType*>(tableArgType.get());
                                         loopVarTypes.push_back(std::make_shared<UnionType>(tupType->elementTypes));
                                   }
                                   handled = true;
                              } else if (tableArgType and tableArgType->kind == Type::Kind::RECORD) {
                                   auto structuralBase = static_cast<const RecordType*>(tableArgType.get())->structuralBase;
                                    if(structuralBase and structuralBase->kind == Type::Kind::ARRAY) {
                                        loopVarTypes.push_back(env.getIntegerType());
                                        loopVarTypes.push_back(static_cast<const ArrayType*>(structuralBase.get())->elementType);
                                         handled = true;
                                    }
                              }
                               if (not handled) {
                                    errors.addError("'ipairs' expects an array, tuple, or record with array part, got '" + (tableArgType ? tableArgType->toString() : "unknown") + "'.", *call->arguments[0]);
                               }
                          }
                     }
                 }
            }

           // Check for `pairs(table)`
           if (not handled and stmt->exprs.size() == 1 and stmt->names.size() <= 2) {
                if(auto *call = dynamic_cast<const ast::FunctionCallExpression*>(stmt->exprs[0].get())) {
                    if(auto *name = dynamic_cast<const ast::NameExpression*>(call->base.get())) {
                         if (name->name == "pairs" and call->arguments.size() == 1) {
                             auto tableArgType = resolveAliases(checkExpression(call->arguments[0].get()));
                             if (tableArgType and tableArgType->isTableLike()) {
                                 if (tableArgType->kind == Type::Kind::MAP) {
                                     const auto *mapType = static_cast<const MapType*>(tableArgType.get());
                                     loopVarTypes.push_back(mapType->keyType);
                                     loopVarTypes.push_back(mapType->valueType);
                                      handled = true;
                                 } else if (tableArgType->kind == Type::Kind::RECORD or tableArgType->kind == Type::Kind::INTERFACE) {
                                     // Pairs over record iterates string keys and any value type (union of field types)
                                      loopVarTypes.push_back(env.getStringType());
                                      // Build union of all field types
                                      std::vector<std::shared_ptr<const Type>> fieldTypes;
                                       std::unordered_map<std::string, FieldInfo> allFields;
                                       if(tableArgType->kind == Type::Kind::RECORD) allFields = static_cast<const RecordType*>(tableArgType.get())->getAllFields();
                                       else allFields = static_cast<const InterfaceType*>(tableArgType.get())->getAllFields();

                                       for(const auto &pair : allFields) {
                                           fieldTypes.push_back(pair.second.type);
                                       }
                                        if(fieldTypes.empty()) loopVarTypes.push_back(env.getNilType()); // Empty record?
                                        else if (fieldTypes.size() == 1) loopVarTypes.push_back(fieldTypes[0]);
                                        else loopVarTypes.push_back(std::make_shared<UnionType>(fieldTypes));

                                       handled = true;
                                 }
                                  // Pairs over array/tuple also works, iterates indices/values like ipairs
                                  else if (tableArgType->kind == Type::Kind::ARRAY or tableArgType->kind == Type::Kind::TUPLE) {
                                      // Similar logic to ipairs
                                       loopVarTypes.push_back(env.getIntegerType()); // Index 'i' (number for pairs?) Lua manual says numeric keys.
                                        if (tableArgType->kind == Type::Kind::ARRAY) {
                                            loopVarTypes.push_back(static_cast<const ArrayType*>(tableArgType.get())->elementType);
                                        } else { // Tuple
                                             const auto *tupType = static_cast<const TupleType*>(tableArgType.get());
                                             loopVarTypes.push_back(std::make_shared<UnionType>(tupType->elementTypes));
                                        }
                                        handled = true;
                                  }
                             }
                              if (not handled) {
                                   errors.addError("'pairs' expects a table-like type, got '" + (tableArgType ? tableArgType->toString() : "unknown") + "'.", *call->arguments[0]);
                              }
                         }
                    }
                }
           }


           // General case: Need type signature of the iterator function
           if (not handled) {
               // Assume first expr is iterator factory, check its return type(s)
               // This requires a more complex representation of iterators, e.g.
               // function(): (function(S, V): (V...), S, V)
               // Where S is state, V is control variable.
               errors.addError("Type checking for custom iterators in for..in loops is not yet fully implemented.", *stmt);
                // Assign 'any' to loop variables for now
                for(size_t i=0; i < stmt->names.size(); ++i) {
                    loopVarTypes.push_back(env.getAnyType());
                }
           }

            // --- Define loop variables and check body ---
            env.pushScope();
            bool defineError = false;
            if (stmt->names.size() > loopVarTypes.size()) {
                 errors.addError("Too many variables for iterator: expected at most " + std::to_string(loopVarTypes.size()) + ", got " + std::to_string(stmt->names.size()) + ".", *stmt);
                 // Pad with nil? Or error out? Error for now.
                 defineError = true;
            }

            for (size_t i = 0; i < stmt->names.size(); ++i) {
                 std::shared_ptr<const Type> varType = (i < loopVarTypes.size()) ? loopVarTypes[i] : env.getNilType(); // Pad with nil if too few iterator returns
                 if (not env.defineValue(stmt->names[i], varType, false, stmt)) { // Loop variables are const-like
                    defineError = true;
                 }
            }

            if (not defineError) {
                 checkBlock(stmt->body.get(), false);
            }

            env.popScope();
      }


      // --- Helper for building FunctionType ---
      std::shared_ptr<const FunctionType> TypeChecker::buildFunctionType(const ast::FunctionBody &body, const std::string &funcName) {
           // TODO: Handle generics (body.type_parameters) later
            if (not body.type_parameters.empty()) {
                 errors.addError("Generics are not yet supported.", body);
            }

           std::vector<FunctionType::Parameter> params;
           std::vector<std::shared_ptr<const Type>> returnTypes;
           bool error = false;

           params.reserve(body.parameters.size());
           for (const auto &p : body.parameters) {
                // TODO: Handle vararg param later (p.is_varadict)
                auto pType = resolveTypeNode(p.type.get());
                if (pType == env.getUnknownType()) error = true;
                params.push_back({ p.name, pType, p.is_optional });
           }

           returnTypes.reserve(body.return_types.size());
            for (const auto &rt : body.return_types) {
                 auto rType = resolveTypeNode(rt.get());
                 if (rType == env.getUnknownType()) error = true;
                 returnTypes.push_back(rType);
            }
             // TODO: Handle vararg return (body.varadict_return) later

            if (error) {
                 return std::static_pointer_cast<const FunctionType>(env.getUnknownType()); // Cast to expected pointer type
            }

            if (not funcName.empty()) {
                 return std::make_shared<FunctionType>(funcName, std::move(params), std::move(returnTypes));
            } else {
                 return std::make_shared<FunctionType>(std::move(params), std::move(returnTypes));
            }
      }

      std::shared_ptr<const FunctionType> TypeChecker::buildFunctionType(const ast::FunctionTypeNode &typeNode) {
            // TODO: Handle generics (typeNode.type_parameters) later
             if (not typeNode.type_parameters.empty()) {
                 errors.addError("Generics are not yet supported.", typeNode);
            }

            std::vector<FunctionType::Parameter> params;
            std::vector<std::shared_ptr<const Type>> returnTypes;
            bool error = false;

            params.reserve(typeNode.parameters.size());
            for (const auto &p : typeNode.parameters) {
                 auto pType = resolveTypeNode(p.type.get());
                 if (pType == env.getUnknownType()) error = true;
                 params.push_back({ p.name, pType, p.is_optional });
            }

            returnTypes.reserve(typeNode.return_types.size());
             for (const auto &rt : typeNode.return_types) {
                  auto rType = resolveTypeNode(rt.get());
                  if (rType == env.getUnknownType()) error = true;
                  returnTypes.push_back(rType);
             }
              // TODO: Handle vararg return (typeNode.varadict_return) later

             if (error) {
                  return std::static_pointer_cast<const FunctionType>(env.getUnknownType()); // Cast to expected pointer type
             }
             return std::make_shared<const FunctionType>(std::move(params), std::move(returnTypes));
      }


    // --- Helper for processing record/interface bodies ---
    template <typename T> // T is RecordType or InterfaceType
    void processRecordBodyInternal(TypeChecker &self, const ast::RecordBody &body, T *targetType) {
         // TODO: Handle generics (body.type_parameters) later
         if (not body.type_parameters.empty()) {
             self.errors.addError("Generics are not yet supported.", body);
         }

         // --- Handle Inheritance ('is' clauses) ---
         // Structural base (e.g., `is {T}`) - Only for Records
         if constexpr (std::is_same_v<T, RecordType>) {
            if (body.structural_ext) {
                 auto baseType = self.resolveTypeNode(body.structural_ext.get());
                 // Expecting an array or tuple type usually
                 if (baseType->kind == Type::Kind::ARRAY or baseType->kind == Type::Kind::TUPLE) {
                    targetType->structuralBase = baseType;
                 } else if(baseType != self.env.getUnknownType()){
                    self.errors.addError("Structural extension ('is {...}') currently only supports array or tuple types.", *body.structural_ext);
                 }
            }
         }

         // Interface inheritance/implementation
         for (const auto &ifaceNode : body.interface_ext) {
            auto ifaceType = self.resolveTypeNode(ifaceNode.get());
             if (ifaceType == self.env.getUnknownType()) continue; // Error already reported

             auto realIfaceType = resolveAliases(ifaceType);
             if (not realIfaceType or realIfaceType->kind != Type::Kind::INTERFACE) {
                 self.errors.addError("Type '" + ifaceType->toString() + "' used in 'is' clause is not an interface.", *ifaceNode);
                 continue;
             }
             auto resolvedIface = std::static_pointer_cast<const InterfaceType>(realIfaceType);

             if constexpr (std::is_same_v<T, RecordType>) {
                  targetType->implementedInterfaces.push_back(resolvedIface);
             } else { // InterfaceType
                  targetType->baseInterfaces.push_back(resolvedIface);
             }
         }

         // --- Handle Where Clause ---
         if (body.where_clause) {
              if constexpr (std::is_same_v<T, RecordType>) {
                   targetType->whereClause = body.where_clause.get();
                   // TODO: Type check the where clause expression itself? Requires context.
              } else {
                   self.errors.addError("'where' clause can only be used in record definitions, not interfaces.", body);
              }
         }


         // --- Handle Entries (Fields, Methods, Nested Types) ---
          for (const auto &entry : body.entries) {
               // TODO: Handle metamethods (entry.is_metamethod) later
               if(entry.is_metamethod) {
                    self.errors.addError("Metamethod declarations are not yet supported.", body); // Need location of specific entry
                    continue;
               }

               switch(entry.entry_kind) {
                   case ast::RecordBody::Entry::Kind::FIELD: {
                        std::string fieldName;
                        if (entry.name) {
                             fieldName = *entry.name;
                        } else if (entry.key_literal) {
                             fieldName = *entry.key_literal; // TODO: Handle string literal parsing if needed '[ "key" ]'
                        } else {
                             self.errors.addError("Internal error: Record field entry has no name or key literal.", body); // Need entry location
                             continue;
                        }

                        if (targetType->fields.count(fieldName)) {
                             self.errors.addError("Field '" + fieldName + "' already defined in this record/interface.", body); // Need entry location
                             continue;
                        }

                        auto fieldType = self.resolveTypeNode(entry.type.get());
                        if (fieldType != self.env.getUnknownType()) {
                             targetType->fields[fieldName] = { fieldType };
                        }
                       break;
                   }
                   case ast::RecordBody::Entry::Kind::USERDATA:
                        self.errors.addError("'userdata' fields/entries are not yet supported.", body); // Need entry location
                        break;
                    case ast::RecordBody::Entry::Kind::TYPE_ALIAS:
                         // Nested type alias
                         self.errors.addError("Nested type alias definitions are not yet fully supported.", body); // Need entry location
                         // If supported:
                         // auto nestedAliasType = self.resolveTypeNode(entry.type_value.get());
                         // Define entry.type_name = TypeAlias(..., nestedAliasType) in the record's *type *scope
                         break;
                     case ast::RecordBody::Entry::Kind::RECORD:
                     case ast::RecordBody::Entry::Kind::ENUM:
                     case ast::RecordBody::Entry::Kind::INTERFACE:
                          // Nested record/enum/interface definition
                          self.errors.addError("Nested record/enum/interface definitions are not yet fully supported.", body); // Need entry location
                           // If supported: Need to handle nested scopes and definitions correctly.
                          // Define entry.nested_name = ... in the record's *type *scope
                          break;
               }
          }

          // TODO: After processing all fields and inheritance, check for conflicts (e.g., same field name inherited with different types)
    }


      void TypeChecker::processRecordBody(const ast::RecordBody &body, RecordType *record) {
         processRecordBodyInternal(*this, body, record);
      }
       void TypeChecker::processRecordBody(const ast::RecordBody &body, InterfaceType *iface) {
          processRecordBodyInternal(*this, body, iface);
      }


      // --- Declaration Checking ---

      void TypeChecker::checkFunctionDeclarationStatement(ast::FunctionDeclarationStatement *stmt) {
            // Function name can be simple `f`, path `a.b.c`, or method `a.b:c`

            std::shared_ptr<const FunctionType> funcType = buildFunctionType(*stmt->body, stmt->method_name.empty() ? (stmt->name_path.empty() ? "<anonymous>" : stmt->name_path.back()) : stmt->method_name);
            if (funcType == env.getUnknownType()) {
                return; // Error during type resolution
            }

             // Define the function in the environment
             bool defineOk = false;
             if (stmt->is_method) {
                  // Method: Need to find the record/interface type for name_path
                  if (stmt->name_path.empty()) {
                      errors.addError("Method declaration ':" + stmt->method_name + "' requires a base table name.", *stmt);
                      return;
                  }
                   auto baseSymbol = env.lookupType(stmt->name_path);
                   if (not baseSymbol or not baseSymbol->type or !(baseSymbol->type->kind == Type::Kind::RECORD or baseSymbol->type->kind == Type::Kind::INTERFACE)) {
                       errors.addError("Base '" + baseSymbol->name + "' for method ':" + stmt->method_name + "' is not a defined record or interface type.", *stmt);
                       return;
                   }
                   // Add method to the record/interface type definition's fields
                   // Requires the RecordType/InterfaceType to be mutable or a way to update it.
                   // This complicates the const-correctness of the Type system.
                   // Alternative: Check if method signature matches an existing field declaration.
                    std::optional<FieldInfo> existingField;
                    if(baseSymbol->type->kind == Type::Kind::RECORD) existingField = static_cast<const RecordType*>(baseSymbol->type.get())->findField(stmt->method_name);
                    else existingField = static_cast<const InterfaceType*>(baseSymbol->type.get())->findField(stmt->method_name);

                    if(not existingField) {
                         errors.addError("Method '" + stmt->method_name + "' is not declared in type '" + baseSymbol->name + "'. Define fields within the record/interface.", *stmt);
                         // Allow definition anyway? Let's disallow for now.
                         return;
                    } else {
                         // Check if the defined function is compatible with the declared field type
                         // Need to consider the implicit 'self' parameter
                         if(funcType->parameters.empty()) {
                             errors.addError("Method implementation for '" + stmt->method_name + "' is missing the 'self' parameter.", *stmt->body);
                             return;
                         }
                         // Create a compatible function type signature including self
                          auto expectedMethodType = resolveAliases(existingField->type);
                          if(not expectedMethodType or expectedMethodType->kind != Type::Kind::FUNCTION) {
                               errors.addError("Field '" + stmt->method_name + "' in type '" + baseSymbol->name + "' is not a function.", *stmt);
                               return;
                          }
                          auto expectedFuncType = std::static_pointer_cast<const FunctionType>(expectedMethodType);

                          // Compare funcType (implementation) with expectedFuncType (declaration)
                          // Implementation needs self param, check against baseSymbol->type
                          checkAssignmentCompat(baseSymbol->type, funcType->parameters[0].type, *stmt->body); // Check self param type

                          // Check remaining params/returns (adjusting for self)
                           // Create temporary types without self for comparison
                           std::vector<FunctionType::Parameter> implParams(funcType->parameters.begin() + 1, funcType->parameters.end());
                           auto implTypeWithoutSelf = std::make_shared<FunctionType>(std::move(implParams), funcType->returnTypes);

                           if(not isAssignable(expectedFuncType, implTypeWithoutSelf)) {
                                errors.addError("Method implementation '" + stmt->method_name + "' signature mismatch. Expected '" + expectedFuncType->toString() + "', got implementation signature '" + implTypeWithoutSelf->toString() + "'.", *stmt);
                           }

                          // If compatible, no need to redefine, just check body.
                          defineOk = true; // Allow body check
                    }

             } else {
                  // Regular function a.b.c = function... or local function f()...
                  if (stmt->name_path.empty()) {
                        errors.addError("Function declaration is missing a name.", *stmt);
                        return;
                  }
                   // TODO: Handle nested paths a.b.c = ... -> requires assigning to field 'c' of table 'a.b'
                   if (stmt->name_path.size() > 1) {
                       errors.addError("Assigning to nested paths (e.g., 'table.field = function...') in function declarations is not yet supported. Use assignment statement.", *stmt);
                       return;
                   }
                  // Simple case: local function f() / global function f()
                   std::string funcName = stmt->name_path.back();
                  if (stmt->visibility == ast::Visibility::LOCAL) {
                        defineOk = env.defineValue(funcName, funcType, false, stmt);
                  } else { // GLOBAL or NONE (assume global for top-level)
                       // Need global scope access, assume defineValue defaults to current scope (which is global here)
                        defineOk = env.defineValue(funcName, funcType, false, stmt);
                  }
             }


             // Check the function body if definition was successful
             if (defineOk) {
                  env.pushScope();
                  // Define params
                  bool paramError = false;
                   for(const auto &param : funcType->parameters) {
                        if (param.name) {
                             if (not env.defineValue(*param.name, param.type, true, stmt->body.get())) {
                                paramError = true; break;
                             }
                        }
                   }
                  // TODO: Define varargs '...' symbol

                  if (not paramError) {
                        env.enterFunction({funcType->returnTypes});
                        checkBlock(stmt->body->body.get(), false);
                        env.exitFunction();
                  }
                  env.popScope();
             }
      }

      void TypeChecker::checkVariableDeclarationStatement(const ast::VariableDeclarationStatement *stmt) {
          size_t n_names = stmt->names.size();
          size_t n_types = stmt->types.size();
          size_t n_values = stmt->values.size();

          if (n_names == 0) {
               errors.addError("Variable declaration requires at least one variable name.", *stmt);
               return;
          }

           // Check attributes <...> - only <const> is relevant for type checking mutability
           std::vector<bool> isMutable(n_names, true);
           for(size_t i=0; i<n_names; ++i) {
               if (stmt->names[i].attribute.has_value()) {
                    if (stmt->names[i].attribute.value() == "const") {
                        isMutable[i] = false;
                    } else if (stmt->names[i].attribute.value() == "close") {
                         // Requires Lua 5.4 target - ignore for basic type check
                    } else if (stmt->names[i].attribute.value() == "total") {
                         // Requires checking table literal completeness - TODO later
                         errors.addError("Variable attribute '<total>' is not yet supported.", *stmt);
                    } else {
                         errors.addError("Unknown variable attribute '<" + stmt->names[i].attribute.value() + ">'.", *stmt);
                    }
               }
                // `<const>` requires initialization
                if (not isMutable[i] and n_values == 0) { // Check if *this specific variable *gets a value
                      // Need to handle multi-assignment alignment
                      if (i >= n_values) {
                         errors.addError("Constant variable '" + stmt->names[i].name + "' must be initialized.", *stmt);
                      }
                }
           }


          for (size_t i = 0; i < n_names; ++i) {
              const std::string &name = stmt->names[i].name;
              std::shared_ptr<const Type> declaredType = nullptr;
              std::shared_ptr<const Type> valueType = nullptr;
              std::shared_ptr<const Type> finalType = nullptr;
              const ast::Expression *valueNode = nullptr;

               // 1. Get Declared Type (if provided)
              if (n_types > 0) {
                  if (n_types == 1) { // `local x, y: type`
                      declaredType = resolveTypeNode(stmt->types[0].get());
                  } else if (i < n_types) { // `local x: t1, y: t2`
                       declaredType = resolveTypeNode(stmt->types[i].get());
                  } else {
                       errors.addError("Mismatch between number of variable names and type annotations.", *stmt);
                       declaredType = env.getUnknownType(); // Error state
                  }
              }

              // 2. Get Value Type (if provided)
               if (i < n_values) {
                    valueNode = stmt->values[i].get();
                    valueType = checkExpressionInternal(valueNode, declaredType);

                    // Handle multi-return from last value in multi-assignment
                    if (i == n_names - 1 and i < n_values and valueType->kind == Type::Kind::MULTI_RETURN) {
                         auto multi = std::static_pointer_cast<const MultiReturnType>(valueType);
                         // Assign first return to current var, remaining to subsequent vars
                         if (not multi->types.empty()) {
                             valueType = multi->types[0];
                             // Need to handle assignment of multi->types[1]... to names[i+1]...
                             // This loop structure doesn't handle multi-return assignment easily.
                             // Let's disallow multi-return in multi-assignment for now.
                              if (n_names > 1) {
                                   errors.addError("Cannot assign multiple return values in a multi-variable declaration.", *valueNode);
                                   valueType = env.getUnknownType(); // Error
                              }
                         } else {
                             valueType = env.getNilType(); // Function returned nothing
                         }
                    }
              }


              // 3. Determine Final Type and Check Compatibility
               if (declaredType and valueType) { // Both type and value given
                    if (declaredType == env.getUnknownType() or valueType == env.getUnknownType()) {
                        finalType = env.getUnknownType(); // Propagate error
                    } else {
                         checkAssignmentCompat(declaredType, valueType, valueNode ? *(ast::ASTNode *)valueNode : *(ast::ASTNode *)stmt);
                         finalType = declaredType; // Declaration takes precedence
                    }
               } else if (declaredType) { // Only type given
                   finalType = declaredType;
               } else if (valueType) { // Only value given (type inference)
                   finalType = valueType;
                    // Handle inference for `local x = {}` - needs context or fails
                    if (finalType->kind == Type::Kind::ARRAY) {
                         auto arrayType = std::static_pointer_cast<const ArrayType>(finalType);
                         if(arrayType->elementType->isAny()) { // Inferred from `{}`
                              errors.addError("Cannot infer type of empty table literal '{}'. Use an explicit type annotation or cast.", valueNode ? *(ast::ASTNode *)valueNode : *(ast::ASTNode *)stmt);
                              finalType = env.getUnknownType();
                         }
                    }

               } else { // Neither type nor value given
                   errors.addError("Variable '" + name + "' must have a type annotation or be initialized.", *stmt);
                   finalType = env.getUnknownType();
               }

               if (finalType == env.getUnknownType()) {
                   // Define as Unknown to prevent cascade errors, but error was reported.
                   env.defineValue(name, env.getUnknownType(), isMutable[i], stmt);
               } else {
                    if (stmt->visibility == ast::Visibility::LOCAL) {
                         env.defineValue(name, finalType, isMutable[i], stmt);
                    } else { // GLOBAL or NONE
                        // TODO: Access global scope explicitly if needed
                         env.defineValue(name, finalType, isMutable[i], stmt);
                    }
               }

          } // End loop over names

           // TODO: Handle <total> attribute check if a table literal was assigned.
      }

    void TypeChecker::checkRecordDeclarationStatement(ast::RecordDeclarationStatement *stmt) {
         // --- Pass 1: Forward Declare ---
          // Define the name in the type scope *before *processing the body to allow recursion.
          // Create a placeholder type (Record or Interface).
          std::shared_ptr<Type> placeholderType;
           if (stmt->is_interface) {
                placeholderType = std::make_shared<InterfaceType>(stmt->name, stmt->body.get());
           } else {
                placeholderType = std::make_shared<RecordType>(stmt->name, stmt->body.get());
           }

           // Define (or forward-declare) in the appropriate scope
            bool declared = false;
            if (stmt->visibility == ast::Visibility::LOCAL) {
                 declared = env.defineType(stmt->name, placeholderType, stmt);
            } else { // GLOBAL or NONE
                 declared = env.defineType(stmt->name, placeholderType, stmt);
                 // TODO: Handle global scope explicitly if Environment separates them
            }

            if (not declared and env.lookupType(stmt->name)) {
                 // Type already fully defined, this is a redefinition error (caught by defineType)
                 // OR it was forward declared, and defineType updated it - check if it matches kind.
                 auto existing = env.lookupType(stmt->name);
                 if(existing and existing->type) {
                      bool kindMatch = (stmt->is_interface and existing->type->kind == Type::Kind::INTERFACE) or
                                      (not stmt->is_interface and existing->type->kind == Type::Kind::RECORD);
                      if(not kindMatch) {
                            errors.addError("Redefinition of '" + stmt->name + "' with different kind (record/interface).", *stmt);
                            return; // Stop processing this declaration
                      }
                      // If kind matches, defineType handled the update, proceed to process body.
                      placeholderType = std::const_pointer_cast<Type>(existing->type); // Get the (potentially updated) pointer
                 } else {
                     // Error occurred during definition/lookup
                     return;
                 }

            } else if (not declared) {
                 // Some other error prevented definition
                 return;
            }


          // --- Pass 2: Process Body ---
           // Now that the name exists, process the body to fill in fields, inheritance etc.
           // We need a *mutable *version of the type during processing.
           // This requires careful handling of the TypeSystem's constness or temporary mutable objects.
           // Let's work on the shared_ptr we got/created. This assumes defineType doesn't make deep copies.
           // It's generally better if Type objects are immutable once fully defined.
           // Workaround: Create a mutable copy, process it, then update the environment symbol?

            std::shared_ptr<Type> typeToProcess = placeholderType; // The pointer held by the SymbolInfo

            // Enter a temporary scope for resolving nested items if needed? No, lookup should handle qualified names.
            // env.pushScope(); ... env.popScope();

            if (stmt->is_interface) {
                 auto iface = std::static_pointer_cast<InterfaceType>(typeToProcess);
                 // Need mutable access to iface fields/bases. Cast away const *carefully *or redesign.
                 // Let's assume InterfaceType/RecordType have methods to add fields/bases if needed,
                 // OR processRecordBody takes a non-const pointer.
                 // The SymbolInfo holds shared_ptr<const Type>, we need to update the pointed-to object.
                 // This implies the object itself isn't const, just the view through the shared_ptr.
                 // Let's cast for now, but this is a design smell.
                  processRecordBody(*stmt->body, const_cast<InterfaceType*>(iface.get()));

            } else {
                 auto record = std::static_pointer_cast<RecordType>(typeToProcess);
                  processRecordBody(*stmt->body, const_cast<RecordType*>(record.get()));
            }

           // --- Pass 3: Validation (Optional) ---
           // Check for conflicts after inheritance, etc.
           // Example: Check if all interface methods are implemented (if applicable)
    }

     void TypeChecker::checkEnumDeclarationStatement(const ast::EnumDeclarationStatement *stmt) {
         // Check for duplicate members
         std::set<std::string> memberSet;
         bool duplicate = false;
         for(const auto &member : stmt->body->elements) {
             if (not memberSet.insert(member).second) {
                 errors.addError("Duplicate member '" + member + "' in enum '" + stmt->name + "'.", *stmt->body);
                 duplicate = true;
             }
         }
         if (duplicate) return; // Don't define if duplicates found

          auto enumType = std::make_shared<EnumType>(stmt->name, stmt->body->elements);

          // Define the Enum type itself
           bool defined = false;
            if (stmt->visibility == ast::Visibility::LOCAL) {
                 defined = env.defineType(stmt->name, enumType, stmt);
            } else { // GLOBAL or NONE
                 defined = env.defineType(stmt->name, enumType, stmt);
            }

            (void)defined;
           // Define enum members as constants? The guide doesn't explicitly say members
           // become global constants. Usage seems to be `var: MyEnum = "member"`.
           // So, just defining the type seems sufficient. String literals are checked
           // against the EnumType during assignment/comparison.
     }

      void TypeChecker::checkTypeAliasStatement(const ast::TypeAliasStatement *stmt) {
           // TODO: Handle generics (stmt->type_parameters) later
            if (not stmt->type_parameters.empty()) {
                 errors.addError("Generics are not yet supported.", *stmt);
            }

           auto targetType = resolveTypeNode(stmt->type.get());
           if (targetType == env.getUnknownType()) {
               return; // Error resolving target type
           }

           auto aliasType = std::make_shared<TypeAlias>(stmt->name, targetType);

           // Define the alias in the type scope
            bool defined = false;
             if (stmt->visibility == ast::Visibility::LOCAL) {
                 defined = env.defineType(stmt->name, aliasType, stmt);
             } else { // GLOBAL or NONE
                 defined = env.defineType(stmt->name, aliasType, stmt);
             }

             (void)defined;
      }

      void TypeChecker::checkAssignmentStatement(const ast::AssignmentStatement *stmt) {
        size_t n_left = stmt->left.size();
        // Store the original number of RHS AST nodes
        size_t original_n_right = stmt->right.size();
    
        if (n_left == 0) return; // Should be caught by parser
    
        std::vector<std::shared_ptr<const Type>> leftTypes;
        std::vector<std::shared_ptr<const Type>> rightTypes; // Will hold the final types after multi-return expansion
        leftTypes.reserve(n_left);
        // Reserve potentially more space if multi-return happens often
        rightTypes.reserve(std::max(n_left, original_n_right));
    
        // --- Step 1: Check left-hand side (must be L-Values) and get their types ---
        bool lvalueError = false;
        for (const auto &leftExpr : stmt->left) {
            if (not isLValue(leftExpr.get())) {
                errors.addError("Invalid target for assignment (must be variable, table index, or field).", *leftExpr);
                lvalueError = true;
            }
            // Check expression even if not LValue to potentially find other errors, but store type
            leftTypes.push_back(checkExpression(leftExpr.get()));
        }
        if (lvalueError) return; // Stop if LHS has invalid targets
    
        // --- Step 2: Check right-hand side expressions *once* and get their types ---
        for (size_t i = 0; i < original_n_right; ++i) {
            // Pass corresponding leftType as hint (if available and not error)
            std::shared_ptr<const Type> hint = (i < n_left) ? leftTypes[i] : nullptr;
            // Ensure hint itself is valid before using it
            if (hint && hint == env.getUnknownType()) {
                 hint = nullptr; // Don't hint if LHS had error or is unknown
            }
            rightTypes.push_back(checkExpressionInternal(stmt->right[i].get(), hint));
        }
    
        // --- Step 3: Handle multi-return from the *last* right-hand expression ---
        size_t current_n_right = rightTypes.size(); // Number of types resolved so far
        if (original_n_right > 0 && !rightTypes.empty() && rightTypes.back()->kind == Type::Kind::MULTI_RETURN) {
            // Ensure the MultiReturnType is correctly formed before casting
            auto multiReturnTypeBase = rightTypes.back();
            if (auto multi = std::dynamic_pointer_cast<const MultiReturnType>(multiReturnTypeBase)) {
                 rightTypes.pop_back(); // Remove the MultiReturnType itself
                 rightTypes.insert(rightTypes.end(), multi->types.begin(), multi->types.end());
                 current_n_right = rightTypes.size(); // Update the count of *resolved types*
            } else {
                // This case should ideally not happen if checkExpression returns MultiReturnType correctly
                errors.addError("Internal error: Expected MultiReturnType but got different type.", *stmt->right.back());
                // Keep rightTypes as is, proceed with caution or return
            }
        }
    
        // --- Step 4: Check compatibility and assign types ---
        for (size_t i = 0; i < n_left; ++i) {
            std::shared_ptr<const Type> targetType = leftTypes[i];
            // Pad with nil if fewer value *types* than targets
            std::shared_ptr<const Type> valueType = (i < current_n_right) ? rightTypes[i] : env.getNilType();
    
            // Determine the correct AST node for error reporting
            const ast::ASTNode *errorNodePtr = nullptr;
            if (i < original_n_right) {
                // This value corresponds to an original RHS expression
                errorNodePtr = stmt->right[i].get();
            } else if (original_n_right > 0) {
                // This value came from the multi-return expansion of the last original RHS expression
                errorNodePtr = stmt->right.back().get();
            } else {
                // No RHS expressions originally, assigning nil implicitly
                errorNodePtr = stmt; // Point to the whole statement
            }
            // Ensure errorNodePtr is not null before dereferencing
            const ast::ASTNode &errorNode = errorNodePtr ? *errorNodePtr : *stmt;
    
    
            if (targetType == env.getUnknownType() or valueType == env.getUnknownType()) {
                continue; // Error already reported for one side
            }
    
            checkAssignmentCompat(targetType, valueType, errorNode);
    
            // Check assignment to const variable (if LHS is a NameExpression)
            if (auto *nameExpr = dynamic_cast<const ast::NameExpression*>(stmt->left[i].get())) {
                auto symbol = env.lookupValue(nameExpr->name);
                if (symbol and not symbol->isMutable) {
                    // Don't report error if assigning nil during declaration? No, const requires init.
                    errors.addError("Cannot assign to constant variable '" + nameExpr->name + "'.", *nameExpr);
                }
            }
            // TODO: Add check for assigning to const fields if records support them
        }
    }

     void TypeChecker::checkCallStatement(const ast::CallStatement *stmt) {
         // Check the function call expression, discard the result type.
         checkFunctionCallExpression(stmt->call.get());
     }

     // --- Helper Implementations ---

      void TypeChecker::checkAssignmentCompat(const std::shared_ptr<const Type>& target, const std::shared_ptr<const Type>& value, const ast::ASTNode &errorNode) {
           if (not target or not value or target == env.getUnknownType() or value == env.getUnknownType()) {
               return; // Skip check if types are invalid/unknown (error already reported)
           }

           if (not isAssignable(target, value)) {
                errors.addError("Type mismatch: cannot assign type '" + value->toString() + "' to variable '"+target->name.value_or("<unknown>")+"' of type '" + target->toString() + "'.", errorNode);
           }
      }

      bool TypeChecker::isLValue(const ast::Expression *expr) {
            if (not expr) return false;
            return expr->is<ast::NameExpression>() or
                   expr->is<ast::IndexExpression>() or
                   expr->is<ast::FieldExpression>();
      }


       std::vector<std::shared_ptr<const Type>> TypeChecker::checkFunctionArguments(
            const ast::FunctionCallExpression &callNode,
            const FunctionType &funcType,
            const std::shared_ptr<const Type>& selfTypeOpt
        ) {
           size_t expectedParamCount = funcType.parameters.size();
           size_t actualArgCount = callNode.arguments.size();
           size_t requiredParamCount = 0;
           
           for(const auto &p : funcType.parameters) {
               if (not p.isOptional) requiredParamCount++;
           }

           // Adjust counts for implicit self parameter in method calls
           if (selfTypeOpt) {
               if (expectedParamCount == 0) {
                    errors.addError("Method '" + funcType.name.value_or("?") + "' has no parameters declared, cannot receive 'self'.", callNode);
                    return {}; // Return empty vector indicating error
               }
               // Check if self type is compatible with first parameter type
               checkAssignmentCompat(funcType.parameters[0].type, selfTypeOpt, *callNode.base);
               expectedParamCount--; // Don't count self in expected args
               if (requiredParamCount > 0) requiredParamCount--;
           }


           // Check arity (number of arguments)
           // TODO: Handle varargs later
            if (actualArgCount < requiredParamCount) {
                 errors.addError("Too few arguments for function call: expected at least " + std::to_string(requiredParamCount) + ", got " + std::to_string(actualArgCount) + ".", callNode);
                 return {}; // Error
            }
            if (actualArgCount > expectedParamCount) {
                 errors.addError("Too many arguments for function call: expected at most " + std::to_string(expectedParamCount) + ", got " + std::to_string(actualArgCount) + ".", callNode);
                 // Continue checking the args that *do *match params.
            }

            // Check argument types against parameter types
            size_t paramIndexOffset = selfTypeOpt ? 1 : 0;
            size_t checkCount = std::min(actualArgCount, expectedParamCount);

            
            for (size_t i = 0; i < checkCount; ++i) {
                 size_t paramIndex = i + paramIndexOffset;
                 std::shared_ptr<const Type> paramType = funcType.parameters[paramIndex].type;
                 auto argType = checkExpressionInternal(callNode.arguments[i].get(), paramType);
                 checkAssignmentCompat(paramType, argType, *callNode.arguments[i]);
            }

            // Return the function's declared return types
            return funcType.returnTypes;
       }


} // namespace teal::typechecker