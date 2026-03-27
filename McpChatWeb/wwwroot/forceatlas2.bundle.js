var _fa2 = (() => {
  var __getOwnPropNames = Object.getOwnPropertyNames;
  var __commonJS = (cb, mod) => function __require() {
    return mod || (0, cb[__getOwnPropNames(cb)[0]])((mod = { exports: {} }).exports, mod), mod.exports;
  };

  // node_modules/graphology-utils/is-graph.js
  var require_is_graph = __commonJS({
    "node_modules/graphology-utils/is-graph.js"(exports, module) {
      module.exports = function isGraph(value) {
        return value !== null && typeof value === "object" && typeof value.addUndirectedEdgeWithKey === "function" && typeof value.dropNode === "function" && typeof value.multi === "boolean";
      };
    }
  });

  // node_modules/graphology-utils/getters.js
  var require_getters = __commonJS({
    "node_modules/graphology-utils/getters.js"(exports) {
      function coerceWeight(value) {
        if (typeof value !== "number" || isNaN(value)) return 1;
        return value;
      }
      function createNodeValueGetter(nameOrFunction, defaultValue) {
        var getter = {};
        var coerceToDefault = function(v) {
          if (typeof v === "undefined") return defaultValue;
          return v;
        };
        if (typeof defaultValue === "function") coerceToDefault = defaultValue;
        var get = function(attributes) {
          return coerceToDefault(attributes[nameOrFunction]);
        };
        var returnDefault = function() {
          return coerceToDefault(void 0);
        };
        if (typeof nameOrFunction === "string") {
          getter.fromAttributes = get;
          getter.fromGraph = function(graph, node) {
            return get(graph.getNodeAttributes(node));
          };
          getter.fromEntry = function(node, attributes) {
            return get(attributes);
          };
        } else if (typeof nameOrFunction === "function") {
          getter.fromAttributes = function() {
            throw new Error(
              "graphology-utils/getters/createNodeValueGetter: irrelevant usage."
            );
          };
          getter.fromGraph = function(graph, node) {
            return coerceToDefault(
              nameOrFunction(node, graph.getNodeAttributes(node))
            );
          };
          getter.fromEntry = function(node, attributes) {
            return coerceToDefault(nameOrFunction(node, attributes));
          };
        } else {
          getter.fromAttributes = returnDefault;
          getter.fromGraph = returnDefault;
          getter.fromEntry = returnDefault;
        }
        return getter;
      }
      function createEdgeValueGetter(nameOrFunction, defaultValue) {
        var getter = {};
        var coerceToDefault = function(v) {
          if (typeof v === "undefined") return defaultValue;
          return v;
        };
        if (typeof defaultValue === "function") coerceToDefault = defaultValue;
        var get = function(attributes) {
          return coerceToDefault(attributes[nameOrFunction]);
        };
        var returnDefault = function() {
          return coerceToDefault(void 0);
        };
        if (typeof nameOrFunction === "string") {
          getter.fromAttributes = get;
          getter.fromGraph = function(graph, edge) {
            return get(graph.getEdgeAttributes(edge));
          };
          getter.fromEntry = function(edge, attributes) {
            return get(attributes);
          };
          getter.fromPartialEntry = getter.fromEntry;
          getter.fromMinimalEntry = getter.fromEntry;
        } else if (typeof nameOrFunction === "function") {
          getter.fromAttributes = function() {
            throw new Error(
              "graphology-utils/getters/createEdgeValueGetter: irrelevant usage."
            );
          };
          getter.fromGraph = function(graph, edge) {
            var extremities = graph.extremities(edge);
            return coerceToDefault(
              nameOrFunction(
                edge,
                graph.getEdgeAttributes(edge),
                extremities[0],
                extremities[1],
                graph.getNodeAttributes(extremities[0]),
                graph.getNodeAttributes(extremities[1]),
                graph.isUndirected(edge)
              )
            );
          };
          getter.fromEntry = function(e, a, s, t, sa, ta, u) {
            return coerceToDefault(nameOrFunction(e, a, s, t, sa, ta, u));
          };
          getter.fromPartialEntry = function(e, a, s, t) {
            return coerceToDefault(nameOrFunction(e, a, s, t));
          };
          getter.fromMinimalEntry = function(e, a) {
            return coerceToDefault(nameOrFunction(e, a));
          };
        } else {
          getter.fromAttributes = returnDefault;
          getter.fromGraph = returnDefault;
          getter.fromEntry = returnDefault;
          getter.fromMinimalEntry = returnDefault;
        }
        return getter;
      }
      exports.createNodeValueGetter = createNodeValueGetter;
      exports.createEdgeValueGetter = createEdgeValueGetter;
      exports.createEdgeWeightGetter = function(name) {
        return createEdgeValueGetter(name, coerceWeight);
      };
    }
  });

  // node_modules/graphology-layout-forceatlas2/iterate.js
  var require_iterate = __commonJS({
    "node_modules/graphology-layout-forceatlas2/iterate.js"(exports, module) {
      var NODE_X = 0;
      var NODE_Y = 1;
      var NODE_DX = 2;
      var NODE_DY = 3;
      var NODE_OLD_DX = 4;
      var NODE_OLD_DY = 5;
      var NODE_MASS = 6;
      var NODE_CONVERGENCE = 7;
      var NODE_SIZE = 8;
      var NODE_FIXED = 9;
      var EDGE_SOURCE = 0;
      var EDGE_TARGET = 1;
      var EDGE_WEIGHT = 2;
      var REGION_NODE = 0;
      var REGION_CENTER_X = 1;
      var REGION_CENTER_Y = 2;
      var REGION_SIZE = 3;
      var REGION_NEXT_SIBLING = 4;
      var REGION_FIRST_CHILD = 5;
      var REGION_MASS = 6;
      var REGION_MASS_CENTER_X = 7;
      var REGION_MASS_CENTER_Y = 8;
      var SUBDIVISION_ATTEMPTS = 3;
      var PPN = 10;
      var PPE = 3;
      var PPR = 9;
      var MAX_FORCE = 10;
      module.exports = function iterate(options, NodeMatrix, EdgeMatrix) {
        var l, r, n, n1, n2, rn, e, w, g, s;
        var order = NodeMatrix.length, size = EdgeMatrix.length;
        var adjustSizes = options.adjustSizes;
        var thetaSquared = options.barnesHutTheta * options.barnesHutTheta;
        var outboundAttCompensation, coefficient, xDist, yDist, ewc, distance, factor;
        var RegionMatrix = [];
        for (n = 0; n < order; n += PPN) {
          NodeMatrix[n + NODE_OLD_DX] = NodeMatrix[n + NODE_DX];
          NodeMatrix[n + NODE_OLD_DY] = NodeMatrix[n + NODE_DY];
          NodeMatrix[n + NODE_DX] = 0;
          NodeMatrix[n + NODE_DY] = 0;
        }
        if (options.outboundAttractionDistribution) {
          outboundAttCompensation = 0;
          for (n = 0; n < order; n += PPN) {
            outboundAttCompensation += NodeMatrix[n + NODE_MASS];
          }
          outboundAttCompensation /= order / PPN;
        }
        if (options.barnesHutOptimize) {
          var minX = Infinity, maxX = -Infinity, minY = Infinity, maxY = -Infinity, q, q2, subdivisionAttempts;
          for (n = 0; n < order; n += PPN) {
            minX = Math.min(minX, NodeMatrix[n + NODE_X]);
            maxX = Math.max(maxX, NodeMatrix[n + NODE_X]);
            minY = Math.min(minY, NodeMatrix[n + NODE_Y]);
            maxY = Math.max(maxY, NodeMatrix[n + NODE_Y]);
          }
          var dx = maxX - minX, dy = maxY - minY;
          if (dx > dy) {
            minY -= (dx - dy) / 2;
            maxY = minY + dx;
          } else {
            minX -= (dy - dx) / 2;
            maxX = minX + dy;
          }
          RegionMatrix[0 + REGION_NODE] = -1;
          RegionMatrix[0 + REGION_CENTER_X] = (minX + maxX) / 2;
          RegionMatrix[0 + REGION_CENTER_Y] = (minY + maxY) / 2;
          RegionMatrix[0 + REGION_SIZE] = Math.max(maxX - minX, maxY - minY);
          RegionMatrix[0 + REGION_NEXT_SIBLING] = -1;
          RegionMatrix[0 + REGION_FIRST_CHILD] = -1;
          RegionMatrix[0 + REGION_MASS] = 0;
          RegionMatrix[0 + REGION_MASS_CENTER_X] = 0;
          RegionMatrix[0 + REGION_MASS_CENTER_Y] = 0;
          l = 1;
          for (n = 0; n < order; n += PPN) {
            r = 0;
            subdivisionAttempts = SUBDIVISION_ATTEMPTS;
            while (true) {
              if (RegionMatrix[r + REGION_FIRST_CHILD] >= 0) {
                if (NodeMatrix[n + NODE_X] < RegionMatrix[r + REGION_CENTER_X]) {
                  if (NodeMatrix[n + NODE_Y] < RegionMatrix[r + REGION_CENTER_Y]) {
                    q = RegionMatrix[r + REGION_FIRST_CHILD];
                  } else {
                    q = RegionMatrix[r + REGION_FIRST_CHILD] + PPR;
                  }
                } else {
                  if (NodeMatrix[n + NODE_Y] < RegionMatrix[r + REGION_CENTER_Y]) {
                    q = RegionMatrix[r + REGION_FIRST_CHILD] + PPR * 2;
                  } else {
                    q = RegionMatrix[r + REGION_FIRST_CHILD] + PPR * 3;
                  }
                }
                RegionMatrix[r + REGION_MASS_CENTER_X] = (RegionMatrix[r + REGION_MASS_CENTER_X] * RegionMatrix[r + REGION_MASS] + NodeMatrix[n + NODE_X] * NodeMatrix[n + NODE_MASS]) / (RegionMatrix[r + REGION_MASS] + NodeMatrix[n + NODE_MASS]);
                RegionMatrix[r + REGION_MASS_CENTER_Y] = (RegionMatrix[r + REGION_MASS_CENTER_Y] * RegionMatrix[r + REGION_MASS] + NodeMatrix[n + NODE_Y] * NodeMatrix[n + NODE_MASS]) / (RegionMatrix[r + REGION_MASS] + NodeMatrix[n + NODE_MASS]);
                RegionMatrix[r + REGION_MASS] += NodeMatrix[n + NODE_MASS];
                r = q;
                continue;
              } else {
                if (RegionMatrix[r + REGION_NODE] < 0) {
                  RegionMatrix[r + REGION_NODE] = n;
                  break;
                } else {
                  RegionMatrix[r + REGION_FIRST_CHILD] = l * PPR;
                  w = RegionMatrix[r + REGION_SIZE] / 2;
                  g = RegionMatrix[r + REGION_FIRST_CHILD];
                  RegionMatrix[g + REGION_NODE] = -1;
                  RegionMatrix[g + REGION_CENTER_X] = RegionMatrix[r + REGION_CENTER_X] - w;
                  RegionMatrix[g + REGION_CENTER_Y] = RegionMatrix[r + REGION_CENTER_Y] - w;
                  RegionMatrix[g + REGION_SIZE] = w;
                  RegionMatrix[g + REGION_NEXT_SIBLING] = g + PPR;
                  RegionMatrix[g + REGION_FIRST_CHILD] = -1;
                  RegionMatrix[g + REGION_MASS] = 0;
                  RegionMatrix[g + REGION_MASS_CENTER_X] = 0;
                  RegionMatrix[g + REGION_MASS_CENTER_Y] = 0;
                  g += PPR;
                  RegionMatrix[g + REGION_NODE] = -1;
                  RegionMatrix[g + REGION_CENTER_X] = RegionMatrix[r + REGION_CENTER_X] - w;
                  RegionMatrix[g + REGION_CENTER_Y] = RegionMatrix[r + REGION_CENTER_Y] + w;
                  RegionMatrix[g + REGION_SIZE] = w;
                  RegionMatrix[g + REGION_NEXT_SIBLING] = g + PPR;
                  RegionMatrix[g + REGION_FIRST_CHILD] = -1;
                  RegionMatrix[g + REGION_MASS] = 0;
                  RegionMatrix[g + REGION_MASS_CENTER_X] = 0;
                  RegionMatrix[g + REGION_MASS_CENTER_Y] = 0;
                  g += PPR;
                  RegionMatrix[g + REGION_NODE] = -1;
                  RegionMatrix[g + REGION_CENTER_X] = RegionMatrix[r + REGION_CENTER_X] + w;
                  RegionMatrix[g + REGION_CENTER_Y] = RegionMatrix[r + REGION_CENTER_Y] - w;
                  RegionMatrix[g + REGION_SIZE] = w;
                  RegionMatrix[g + REGION_NEXT_SIBLING] = g + PPR;
                  RegionMatrix[g + REGION_FIRST_CHILD] = -1;
                  RegionMatrix[g + REGION_MASS] = 0;
                  RegionMatrix[g + REGION_MASS_CENTER_X] = 0;
                  RegionMatrix[g + REGION_MASS_CENTER_Y] = 0;
                  g += PPR;
                  RegionMatrix[g + REGION_NODE] = -1;
                  RegionMatrix[g + REGION_CENTER_X] = RegionMatrix[r + REGION_CENTER_X] + w;
                  RegionMatrix[g + REGION_CENTER_Y] = RegionMatrix[r + REGION_CENTER_Y] + w;
                  RegionMatrix[g + REGION_SIZE] = w;
                  RegionMatrix[g + REGION_NEXT_SIBLING] = RegionMatrix[r + REGION_NEXT_SIBLING];
                  RegionMatrix[g + REGION_FIRST_CHILD] = -1;
                  RegionMatrix[g + REGION_MASS] = 0;
                  RegionMatrix[g + REGION_MASS_CENTER_X] = 0;
                  RegionMatrix[g + REGION_MASS_CENTER_Y] = 0;
                  l += 4;
                  if (NodeMatrix[RegionMatrix[r + REGION_NODE] + NODE_X] < RegionMatrix[r + REGION_CENTER_X]) {
                    if (NodeMatrix[RegionMatrix[r + REGION_NODE] + NODE_Y] < RegionMatrix[r + REGION_CENTER_Y]) {
                      q = RegionMatrix[r + REGION_FIRST_CHILD];
                    } else {
                      q = RegionMatrix[r + REGION_FIRST_CHILD] + PPR;
                    }
                  } else {
                    if (NodeMatrix[RegionMatrix[r + REGION_NODE] + NODE_Y] < RegionMatrix[r + REGION_CENTER_Y]) {
                      q = RegionMatrix[r + REGION_FIRST_CHILD] + PPR * 2;
                    } else {
                      q = RegionMatrix[r + REGION_FIRST_CHILD] + PPR * 3;
                    }
                  }
                  RegionMatrix[r + REGION_MASS] = NodeMatrix[RegionMatrix[r + REGION_NODE] + NODE_MASS];
                  RegionMatrix[r + REGION_MASS_CENTER_X] = NodeMatrix[RegionMatrix[r + REGION_NODE] + NODE_X];
                  RegionMatrix[r + REGION_MASS_CENTER_Y] = NodeMatrix[RegionMatrix[r + REGION_NODE] + NODE_Y];
                  RegionMatrix[q + REGION_NODE] = RegionMatrix[r + REGION_NODE];
                  RegionMatrix[r + REGION_NODE] = -1;
                  if (NodeMatrix[n + NODE_X] < RegionMatrix[r + REGION_CENTER_X]) {
                    if (NodeMatrix[n + NODE_Y] < RegionMatrix[r + REGION_CENTER_Y]) {
                      q2 = RegionMatrix[r + REGION_FIRST_CHILD];
                    } else {
                      q2 = RegionMatrix[r + REGION_FIRST_CHILD] + PPR;
                    }
                  } else {
                    if (NodeMatrix[n + NODE_Y] < RegionMatrix[r + REGION_CENTER_Y]) {
                      q2 = RegionMatrix[r + REGION_FIRST_CHILD] + PPR * 2;
                    } else {
                      q2 = RegionMatrix[r + REGION_FIRST_CHILD] + PPR * 3;
                    }
                  }
                  if (q === q2) {
                    if (subdivisionAttempts--) {
                      r = q;
                      continue;
                    } else {
                      subdivisionAttempts = SUBDIVISION_ATTEMPTS;
                      break;
                    }
                  }
                  RegionMatrix[q2 + REGION_NODE] = n;
                  break;
                }
              }
            }
          }
        }
        if (options.barnesHutOptimize) {
          coefficient = options.scalingRatio;
          for (n = 0; n < order; n += PPN) {
            r = 0;
            while (true) {
              if (RegionMatrix[r + REGION_FIRST_CHILD] >= 0) {
                distance = Math.pow(
                  NodeMatrix[n + NODE_X] - RegionMatrix[r + REGION_MASS_CENTER_X],
                  2
                ) + Math.pow(
                  NodeMatrix[n + NODE_Y] - RegionMatrix[r + REGION_MASS_CENTER_Y],
                  2
                );
                s = RegionMatrix[r + REGION_SIZE];
                if (4 * s * s / distance < thetaSquared) {
                  xDist = NodeMatrix[n + NODE_X] - RegionMatrix[r + REGION_MASS_CENTER_X];
                  yDist = NodeMatrix[n + NODE_Y] - RegionMatrix[r + REGION_MASS_CENTER_Y];
                  if (adjustSizes === true) {
                    if (distance > 0) {
                      factor = coefficient * NodeMatrix[n + NODE_MASS] * RegionMatrix[r + REGION_MASS] / distance;
                      NodeMatrix[n + NODE_DX] += xDist * factor;
                      NodeMatrix[n + NODE_DY] += yDist * factor;
                    } else if (distance < 0) {
                      factor = -coefficient * NodeMatrix[n + NODE_MASS] * RegionMatrix[r + REGION_MASS] / Math.sqrt(distance);
                      NodeMatrix[n + NODE_DX] += xDist * factor;
                      NodeMatrix[n + NODE_DY] += yDist * factor;
                    }
                  } else {
                    if (distance > 0) {
                      factor = coefficient * NodeMatrix[n + NODE_MASS] * RegionMatrix[r + REGION_MASS] / distance;
                      NodeMatrix[n + NODE_DX] += xDist * factor;
                      NodeMatrix[n + NODE_DY] += yDist * factor;
                    }
                  }
                  r = RegionMatrix[r + REGION_NEXT_SIBLING];
                  if (r < 0) break;
                  continue;
                } else {
                  r = RegionMatrix[r + REGION_FIRST_CHILD];
                  continue;
                }
              } else {
                rn = RegionMatrix[r + REGION_NODE];
                if (rn >= 0 && rn !== n) {
                  xDist = NodeMatrix[n + NODE_X] - NodeMatrix[rn + NODE_X];
                  yDist = NodeMatrix[n + NODE_Y] - NodeMatrix[rn + NODE_Y];
                  distance = xDist * xDist + yDist * yDist;
                  if (adjustSizes === true) {
                    if (distance > 0) {
                      factor = coefficient * NodeMatrix[n + NODE_MASS] * NodeMatrix[rn + NODE_MASS] / distance;
                      NodeMatrix[n + NODE_DX] += xDist * factor;
                      NodeMatrix[n + NODE_DY] += yDist * factor;
                    } else if (distance < 0) {
                      factor = -coefficient * NodeMatrix[n + NODE_MASS] * NodeMatrix[rn + NODE_MASS] / Math.sqrt(distance);
                      NodeMatrix[n + NODE_DX] += xDist * factor;
                      NodeMatrix[n + NODE_DY] += yDist * factor;
                    }
                  } else {
                    if (distance > 0) {
                      factor = coefficient * NodeMatrix[n + NODE_MASS] * NodeMatrix[rn + NODE_MASS] / distance;
                      NodeMatrix[n + NODE_DX] += xDist * factor;
                      NodeMatrix[n + NODE_DY] += yDist * factor;
                    }
                  }
                }
                r = RegionMatrix[r + REGION_NEXT_SIBLING];
                if (r < 0) break;
                continue;
              }
            }
          }
        } else {
          coefficient = options.scalingRatio;
          for (n1 = 0; n1 < order; n1 += PPN) {
            for (n2 = 0; n2 < n1; n2 += PPN) {
              xDist = NodeMatrix[n1 + NODE_X] - NodeMatrix[n2 + NODE_X];
              yDist = NodeMatrix[n1 + NODE_Y] - NodeMatrix[n2 + NODE_Y];
              if (adjustSizes === true) {
                distance = Math.sqrt(xDist * xDist + yDist * yDist) - NodeMatrix[n1 + NODE_SIZE] - NodeMatrix[n2 + NODE_SIZE];
                if (distance > 0) {
                  factor = coefficient * NodeMatrix[n1 + NODE_MASS] * NodeMatrix[n2 + NODE_MASS] / distance / distance;
                  NodeMatrix[n1 + NODE_DX] += xDist * factor;
                  NodeMatrix[n1 + NODE_DY] += yDist * factor;
                  NodeMatrix[n2 + NODE_DX] -= xDist * factor;
                  NodeMatrix[n2 + NODE_DY] -= yDist * factor;
                } else if (distance < 0) {
                  factor = 100 * coefficient * NodeMatrix[n1 + NODE_MASS] * NodeMatrix[n2 + NODE_MASS];
                  NodeMatrix[n1 + NODE_DX] += xDist * factor;
                  NodeMatrix[n1 + NODE_DY] += yDist * factor;
                  NodeMatrix[n2 + NODE_DX] -= xDist * factor;
                  NodeMatrix[n2 + NODE_DY] -= yDist * factor;
                }
              } else {
                distance = Math.sqrt(xDist * xDist + yDist * yDist);
                if (distance > 0) {
                  factor = coefficient * NodeMatrix[n1 + NODE_MASS] * NodeMatrix[n2 + NODE_MASS] / distance / distance;
                  NodeMatrix[n1 + NODE_DX] += xDist * factor;
                  NodeMatrix[n1 + NODE_DY] += yDist * factor;
                  NodeMatrix[n2 + NODE_DX] -= xDist * factor;
                  NodeMatrix[n2 + NODE_DY] -= yDist * factor;
                }
              }
            }
          }
        }
        g = options.gravity / options.scalingRatio;
        coefficient = options.scalingRatio;
        for (n = 0; n < order; n += PPN) {
          factor = 0;
          xDist = NodeMatrix[n + NODE_X];
          yDist = NodeMatrix[n + NODE_Y];
          distance = Math.sqrt(Math.pow(xDist, 2) + Math.pow(yDist, 2));
          if (options.strongGravityMode) {
            if (distance > 0) factor = coefficient * NodeMatrix[n + NODE_MASS] * g;
          } else {
            if (distance > 0)
              factor = coefficient * NodeMatrix[n + NODE_MASS] * g / distance;
          }
          NodeMatrix[n + NODE_DX] -= xDist * factor;
          NodeMatrix[n + NODE_DY] -= yDist * factor;
        }
        coefficient = 1 * (options.outboundAttractionDistribution ? outboundAttCompensation : 1);
        for (e = 0; e < size; e += PPE) {
          n1 = EdgeMatrix[e + EDGE_SOURCE];
          n2 = EdgeMatrix[e + EDGE_TARGET];
          w = EdgeMatrix[e + EDGE_WEIGHT];
          ewc = Math.pow(w, options.edgeWeightInfluence);
          xDist = NodeMatrix[n1 + NODE_X] - NodeMatrix[n2 + NODE_X];
          yDist = NodeMatrix[n1 + NODE_Y] - NodeMatrix[n2 + NODE_Y];
          if (adjustSizes === true) {
            distance = Math.sqrt(xDist * xDist + yDist * yDist) - NodeMatrix[n1 + NODE_SIZE] - NodeMatrix[n2 + NODE_SIZE];
            if (options.linLogMode) {
              if (options.outboundAttractionDistribution) {
                if (distance > 0) {
                  factor = -coefficient * ewc * Math.log(1 + distance) / distance / NodeMatrix[n1 + NODE_MASS];
                }
              } else {
                if (distance > 0) {
                  factor = -coefficient * ewc * Math.log(1 + distance) / distance;
                }
              }
            } else {
              if (options.outboundAttractionDistribution) {
                if (distance > 0) {
                  factor = -coefficient * ewc / NodeMatrix[n1 + NODE_MASS];
                }
              } else {
                if (distance > 0) {
                  factor = -coefficient * ewc;
                }
              }
            }
          } else {
            distance = Math.sqrt(Math.pow(xDist, 2) + Math.pow(yDist, 2));
            if (options.linLogMode) {
              if (options.outboundAttractionDistribution) {
                if (distance > 0) {
                  factor = -coefficient * ewc * Math.log(1 + distance) / distance / NodeMatrix[n1 + NODE_MASS];
                }
              } else {
                if (distance > 0)
                  factor = -coefficient * ewc * Math.log(1 + distance) / distance;
              }
            } else {
              if (options.outboundAttractionDistribution) {
                distance = 1;
                factor = -coefficient * ewc / NodeMatrix[n1 + NODE_MASS];
              } else {
                distance = 1;
                factor = -coefficient * ewc;
              }
            }
          }
          if (distance > 0) {
            NodeMatrix[n1 + NODE_DX] += xDist * factor;
            NodeMatrix[n1 + NODE_DY] += yDist * factor;
            NodeMatrix[n2 + NODE_DX] -= xDist * factor;
            NodeMatrix[n2 + NODE_DY] -= yDist * factor;
          }
        }
        var force, swinging, traction, nodespeed, newX, newY;
        if (adjustSizes === true) {
          for (n = 0; n < order; n += PPN) {
            if (NodeMatrix[n + NODE_FIXED] !== 1) {
              force = Math.sqrt(
                Math.pow(NodeMatrix[n + NODE_DX], 2) + Math.pow(NodeMatrix[n + NODE_DY], 2)
              );
              if (force > MAX_FORCE) {
                NodeMatrix[n + NODE_DX] = NodeMatrix[n + NODE_DX] * MAX_FORCE / force;
                NodeMatrix[n + NODE_DY] = NodeMatrix[n + NODE_DY] * MAX_FORCE / force;
              }
              swinging = NodeMatrix[n + NODE_MASS] * Math.sqrt(
                (NodeMatrix[n + NODE_OLD_DX] - NodeMatrix[n + NODE_DX]) * (NodeMatrix[n + NODE_OLD_DX] - NodeMatrix[n + NODE_DX]) + (NodeMatrix[n + NODE_OLD_DY] - NodeMatrix[n + NODE_DY]) * (NodeMatrix[n + NODE_OLD_DY] - NodeMatrix[n + NODE_DY])
              );
              traction = Math.sqrt(
                (NodeMatrix[n + NODE_OLD_DX] + NodeMatrix[n + NODE_DX]) * (NodeMatrix[n + NODE_OLD_DX] + NodeMatrix[n + NODE_DX]) + (NodeMatrix[n + NODE_OLD_DY] + NodeMatrix[n + NODE_DY]) * (NodeMatrix[n + NODE_OLD_DY] + NodeMatrix[n + NODE_DY])
              ) / 2;
              nodespeed = 0.1 * Math.log(1 + traction) / (1 + Math.sqrt(swinging));
              newX = NodeMatrix[n + NODE_X] + NodeMatrix[n + NODE_DX] * (nodespeed / options.slowDown);
              NodeMatrix[n + NODE_X] = newX;
              newY = NodeMatrix[n + NODE_Y] + NodeMatrix[n + NODE_DY] * (nodespeed / options.slowDown);
              NodeMatrix[n + NODE_Y] = newY;
            }
          }
        } else {
          for (n = 0; n < order; n += PPN) {
            if (NodeMatrix[n + NODE_FIXED] !== 1) {
              swinging = NodeMatrix[n + NODE_MASS] * Math.sqrt(
                (NodeMatrix[n + NODE_OLD_DX] - NodeMatrix[n + NODE_DX]) * (NodeMatrix[n + NODE_OLD_DX] - NodeMatrix[n + NODE_DX]) + (NodeMatrix[n + NODE_OLD_DY] - NodeMatrix[n + NODE_DY]) * (NodeMatrix[n + NODE_OLD_DY] - NodeMatrix[n + NODE_DY])
              );
              traction = Math.sqrt(
                (NodeMatrix[n + NODE_OLD_DX] + NodeMatrix[n + NODE_DX]) * (NodeMatrix[n + NODE_OLD_DX] + NodeMatrix[n + NODE_DX]) + (NodeMatrix[n + NODE_OLD_DY] + NodeMatrix[n + NODE_DY]) * (NodeMatrix[n + NODE_OLD_DY] + NodeMatrix[n + NODE_DY])
              ) / 2;
              nodespeed = NodeMatrix[n + NODE_CONVERGENCE] * Math.log(1 + traction) / (1 + Math.sqrt(swinging));
              NodeMatrix[n + NODE_CONVERGENCE] = Math.min(
                1,
                Math.sqrt(
                  nodespeed * (Math.pow(NodeMatrix[n + NODE_DX], 2) + Math.pow(NodeMatrix[n + NODE_DY], 2)) / (1 + Math.sqrt(swinging))
                )
              );
              newX = NodeMatrix[n + NODE_X] + NodeMatrix[n + NODE_DX] * (nodespeed / options.slowDown);
              NodeMatrix[n + NODE_X] = newX;
              newY = NodeMatrix[n + NODE_Y] + NodeMatrix[n + NODE_DY] * (nodespeed / options.slowDown);
              NodeMatrix[n + NODE_Y] = newY;
            }
          }
        }
        return {};
      };
    }
  });

  // node_modules/graphology-layout-forceatlas2/helpers.js
  var require_helpers = __commonJS({
    "node_modules/graphology-layout-forceatlas2/helpers.js"(exports) {
      var PPN = 10;
      var PPE = 3;
      exports.assign = function(target) {
        target = target || {};
        var objects = Array.prototype.slice.call(arguments).slice(1), i, k, l;
        for (i = 0, l = objects.length; i < l; i++) {
          if (!objects[i]) continue;
          for (k in objects[i]) target[k] = objects[i][k];
        }
        return target;
      };
      exports.validateSettings = function(settings) {
        if ("linLogMode" in settings && typeof settings.linLogMode !== "boolean")
          return { message: "the `linLogMode` setting should be a boolean." };
        if ("outboundAttractionDistribution" in settings && typeof settings.outboundAttractionDistribution !== "boolean")
          return {
            message: "the `outboundAttractionDistribution` setting should be a boolean."
          };
        if ("adjustSizes" in settings && typeof settings.adjustSizes !== "boolean")
          return { message: "the `adjustSizes` setting should be a boolean." };
        if ("edgeWeightInfluence" in settings && typeof settings.edgeWeightInfluence !== "number")
          return {
            message: "the `edgeWeightInfluence` setting should be a number."
          };
        if ("scalingRatio" in settings && !(typeof settings.scalingRatio === "number" && settings.scalingRatio >= 0))
          return { message: "the `scalingRatio` setting should be a number >= 0." };
        if ("strongGravityMode" in settings && typeof settings.strongGravityMode !== "boolean")
          return { message: "the `strongGravityMode` setting should be a boolean." };
        if ("gravity" in settings && !(typeof settings.gravity === "number" && settings.gravity >= 0))
          return { message: "the `gravity` setting should be a number >= 0." };
        if ("slowDown" in settings && !(typeof settings.slowDown === "number" || settings.slowDown >= 0))
          return { message: "the `slowDown` setting should be a number >= 0." };
        if ("barnesHutOptimize" in settings && typeof settings.barnesHutOptimize !== "boolean")
          return { message: "the `barnesHutOptimize` setting should be a boolean." };
        if ("barnesHutTheta" in settings && !(typeof settings.barnesHutTheta === "number" && settings.barnesHutTheta >= 0))
          return { message: "the `barnesHutTheta` setting should be a number >= 0." };
        return null;
      };
      exports.graphToByteArrays = function(graph, getEdgeWeight) {
        var order = graph.order;
        var size = graph.size;
        var index = {};
        var j;
        var NodeMatrix = new Float32Array(order * PPN);
        var EdgeMatrix = new Float32Array(size * PPE);
        j = 0;
        graph.forEachNode(function(node, attr) {
          index[node] = j;
          NodeMatrix[j] = attr.x;
          NodeMatrix[j + 1] = attr.y;
          NodeMatrix[j + 2] = 0;
          NodeMatrix[j + 3] = 0;
          NodeMatrix[j + 4] = 0;
          NodeMatrix[j + 5] = 0;
          NodeMatrix[j + 6] = 1;
          NodeMatrix[j + 7] = 1;
          NodeMatrix[j + 8] = attr.size || 1;
          NodeMatrix[j + 9] = attr.fixed ? 1 : 0;
          j += PPN;
        });
        j = 0;
        graph.forEachEdge(function(edge, attr, source, target, sa, ta, u) {
          var sj = index[source];
          var tj = index[target];
          var weight = getEdgeWeight(edge, attr, source, target, sa, ta, u);
          NodeMatrix[sj + 6] += weight;
          NodeMatrix[tj + 6] += weight;
          EdgeMatrix[j] = sj;
          EdgeMatrix[j + 1] = tj;
          EdgeMatrix[j + 2] = weight;
          j += PPE;
        });
        return {
          nodes: NodeMatrix,
          edges: EdgeMatrix
        };
      };
      exports.assignLayoutChanges = function(graph, NodeMatrix, outputReducer) {
        var i = 0;
        graph.updateEachNodeAttributes(function(node, attr) {
          attr.x = NodeMatrix[i];
          attr.y = NodeMatrix[i + 1];
          i += PPN;
          return outputReducer ? outputReducer(node, attr) : attr;
        });
      };
      exports.readGraphPositions = function(graph, NodeMatrix) {
        var i = 0;
        graph.forEachNode(function(node, attr) {
          NodeMatrix[i] = attr.x;
          NodeMatrix[i + 1] = attr.y;
          i += PPN;
        });
      };
      exports.collectLayoutChanges = function(graph, NodeMatrix, outputReducer) {
        var nodes = graph.nodes(), positions = {};
        for (var i = 0, j = 0, l = NodeMatrix.length; i < l; i += PPN) {
          if (outputReducer) {
            var newAttr = Object.assign({}, graph.getNodeAttributes(nodes[j]));
            newAttr.x = NodeMatrix[i];
            newAttr.y = NodeMatrix[i + 1];
            newAttr = outputReducer(nodes[j], newAttr);
            positions[nodes[j]] = {
              x: newAttr.x,
              y: newAttr.y
            };
          } else {
            positions[nodes[j]] = {
              x: NodeMatrix[i],
              y: NodeMatrix[i + 1]
            };
          }
          j++;
        }
        return positions;
      };
      exports.createWorker = function createWorker(fn) {
        var xURL = window.URL || window.webkitURL;
        var code = fn.toString();
        var objectUrl = xURL.createObjectURL(
          new Blob(["(" + code + ").call(this);"], { type: "text/javascript" })
        );
        var worker = new Worker(objectUrl);
        xURL.revokeObjectURL(objectUrl);
        return worker;
      };
    }
  });

  // node_modules/graphology-layout-forceatlas2/defaults.js
  var require_defaults = __commonJS({
    "node_modules/graphology-layout-forceatlas2/defaults.js"(exports, module) {
      module.exports = {
        linLogMode: false,
        outboundAttractionDistribution: false,
        adjustSizes: false,
        edgeWeightInfluence: 1,
        scalingRatio: 1,
        strongGravityMode: false,
        gravity: 1,
        slowDown: 1,
        barnesHutOptimize: false,
        barnesHutTheta: 0.5
      };
    }
  });

  // node_modules/graphology-layout-forceatlas2/index.js
  var require_graphology_layout_forceatlas2 = __commonJS({
    "node_modules/graphology-layout-forceatlas2/index.js"(exports, module) {
      var isGraph = require_is_graph();
      var createEdgeWeightGetter = require_getters().createEdgeWeightGetter;
      var iterate = require_iterate();
      var helpers = require_helpers();
      var DEFAULT_SETTINGS = require_defaults();
      function abstractSynchronousLayout(assign, graph, params) {
        if (!isGraph(graph))
          throw new Error(
            "graphology-layout-forceatlas2: the given graph is not a valid graphology instance."
          );
        if (typeof params === "number") params = { iterations: params };
        var iterations = params.iterations;
        if (typeof iterations !== "number")
          throw new Error(
            "graphology-layout-forceatlas2: invalid number of iterations."
          );
        if (iterations <= 0)
          throw new Error(
            "graphology-layout-forceatlas2: you should provide a positive number of iterations."
          );
        var getEdgeWeight = createEdgeWeightGetter(
          "getEdgeWeight" in params ? params.getEdgeWeight : "weight"
        ).fromEntry;
        var outputReducer = typeof params.outputReducer === "function" ? params.outputReducer : null;
        var settings = helpers.assign({}, DEFAULT_SETTINGS, params.settings);
        var validationError = helpers.validateSettings(settings);
        if (validationError)
          throw new Error(
            "graphology-layout-forceatlas2: " + validationError.message
          );
        var matrices = helpers.graphToByteArrays(graph, getEdgeWeight);
        var i;
        for (i = 0; i < iterations; i++)
          iterate(settings, matrices.nodes, matrices.edges);
        if (assign) {
          helpers.assignLayoutChanges(graph, matrices.nodes, outputReducer);
          return;
        }
        return helpers.collectLayoutChanges(graph, matrices.nodes);
      }
      function inferSettings(graph) {
        var order = typeof graph === "number" ? graph : graph.order;
        return {
          barnesHutOptimize: order > 2e3,
          strongGravityMode: true,
          gravity: 0.05,
          scalingRatio: 10,
          slowDown: 1 + Math.log(order)
        };
      }
      var synchronousLayout = abstractSynchronousLayout.bind(null, false);
      synchronousLayout.assign = abstractSynchronousLayout.bind(null, true);
      synchronousLayout.inferSettings = inferSettings;
      module.exports = synchronousLayout;
    }
  });

  // forceatlas2-entry.js
  var forceAtlas2 = require_graphology_layout_forceatlas2();
  window.ForceAtlas2Layout = forceAtlas2;
})();
