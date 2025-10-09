/*
 * Copyright 2025 Mostly Codes
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHC WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package mostly.uncertaintee.ops

import mostly.uncertaintee.Uncertain
import mostly.uncertaintee.syntax.functional.*

import scala.util.NotGiven

// --- TYPECLASS DEFINITIONS ---

/** Allows basic common numeric operations on Uncertain[T] types, e.g.
  *
  * {{{
  * (uncertaintyA + 3.5 - uncertaintyB) % uncertaintyC
  * }}}
  *
  * {{{
  * import mostly.uncertaintee.syntax.distribution.*
  * // or just import all the syntax (recommended)
  * import mostly.uncertaintee.syntax.*
  * }}}
  */
trait ArithmeticOps {

  trait Add[A, B]   { type C; def apply(a: A, b: B): C }
  trait Sub[A, B]   { type C; def apply(a: A, b: B): C }
  trait Times[A, B] { type C; def apply(a: A, b: B): C }
  trait Div[A, B]   { type C; def apply(a: A, b: B): C }
  trait Rem[A, B]   { type C; def apply(a: A, b: B): C }

// --- COMPANION OBJECTS WITH GIVEN INSTANCES ---

  given Add[Double, Double] with { type C = Double; def apply(a: Double, b: Double): Double = a + b }
  given Add[Double, Float] with  { type C = Double; def apply(a: Double, b: Float): Double = a + b  }
  given Add[Double, Long] with   { type C = Double; def apply(a: Double, b: Long): Double = a + b   }
  given Add[Double, Int] with    { type C = Double; def apply(a: Double, b: Int): Double = a + b    }
  given Add[Double, Short] with  { type C = Double; def apply(a: Double, b: Short): Double = a + b  }
  given Add[Double, Byte] with   { type C = Double; def apply(a: Double, b: Byte): Double = a + b   }
  given Add[Double, Char] with   { type C = Double; def apply(a: Double, b: Char): Double = a + b   }
  given Add[Float, Double] with  { type C = Double; def apply(a: Float, b: Double): Double = a + b  }
  given Add[Float, Float] with   { type C = Float; def apply(a: Float, b: Float): Float = a + b     }
  given Add[Float, Long] with    { type C = Float; def apply(a: Float, b: Long): Float = a + b      }
  given Add[Float, Int] with     { type C = Float; def apply(a: Float, b: Int): Float = a + b       }
  given Add[Float, Short] with   { type C = Float; def apply(a: Float, b: Short): Float = a + b     }
  given Add[Float, Byte] with    { type C = Float; def apply(a: Float, b: Byte): Float = a + b      }
  given Add[Float, Char] with    { type C = Float; def apply(a: Float, b: Char): Float = a + b      }
  given Add[Long, Double] with   { type C = Double; def apply(a: Long, b: Double): Double = a + b   }
  given Add[Long, Float] with    { type C = Float; def apply(a: Long, b: Float): Float = a + b      }
  given Add[Long, Long] with     { type C = Long; def apply(a: Long, b: Long): Long = a + b         }
  given Add[Long, Int] with      { type C = Long; def apply(a: Long, b: Int): Long = a + b          }
  given Add[Long, Short] with    { type C = Long; def apply(a: Long, b: Short): Long = a + b        }
  given Add[Long, Byte] with     { type C = Long; def apply(a: Long, b: Byte): Long = a + b         }
  given Add[Long, Char] with     { type C = Long; def apply(a: Long, b: Char): Long = a + b         }
  given Add[Int, Double] with    { type C = Double; def apply(a: Int, b: Double): Double = a + b    }
  given Add[Int, Float] with     { type C = Float; def apply(a: Int, b: Float): Float = a + b       }
  given Add[Int, Long] with      { type C = Long; def apply(a: Int, b: Long): Long = a + b          }
  given Add[Int, Int] with       { type C = Int; def apply(a: Int, b: Int): Int = a + b             }
  given Add[Int, Short] with     { type C = Int; def apply(a: Int, b: Short): Int = a + b           }
  given Add[Int, Byte] with      { type C = Int; def apply(a: Int, b: Byte): Int = a + b            }
  given Add[Int, Char] with      { type C = Int; def apply(a: Int, b: Char): Int = a + b            }
  given Add[Short, Double] with  { type C = Double; def apply(a: Short, b: Double): Double = a + b  }
  given Add[Short, Float] with   { type C = Float; def apply(a: Short, b: Float): Float = a + b     }
  given Add[Short, Long] with    { type C = Long; def apply(a: Short, b: Long): Long = a + b        }
  given Add[Short, Int] with     { type C = Int; def apply(a: Short, b: Int): Int = a + b           }
  given Add[Short, Short] with   { type C = Int; def apply(a: Short, b: Short): Int = a + b         }
  given Add[Short, Byte] with    { type C = Int; def apply(a: Short, b: Byte): Int = a + b          }
  given Add[Short, Char] with    { type C = Int; def apply(a: Short, b: Char): Int = a + b          }
  given Add[Byte, Double] with   { type C = Double; def apply(a: Byte, b: Double): Double = a + b   }
  given Add[Byte, Float] with    { type C = Float; def apply(a: Byte, b: Float): Float = a + b      }
  given Add[Byte, Long] with     { type C = Long; def apply(a: Byte, b: Long): Long = a + b         }
  given Add[Byte, Int] with      { type C = Int; def apply(a: Byte, b: Int): Int = a + b            }
  given Add[Byte, Short] with    { type C = Int; def apply(a: Byte, b: Short): Int = a + b          }
  given Add[Byte, Byte] with     { type C = Int; def apply(a: Byte, b: Byte): Int = a + b           }
  given Add[Byte, Char] with     { type C = Int; def apply(a: Byte, b: Char): Int = a + b           }
  given Add[Char, Double] with   { type C = Double; def apply(a: Char, b: Double): Double = a + b   }
  given Add[Char, Float] with    { type C = Float; def apply(a: Char, b: Float): Float = a + b      }
  given Add[Char, Long] with     { type C = Long; def apply(a: Char, b: Long): Long = a + b         }
  given Add[Char, Int] with      { type C = Int; def apply(a: Char, b: Int): Int = a + b            }
  given Add[Char, Short] with    { type C = Int; def apply(a: Char, b: Short): Int = a + b          }
  given Add[Char, Byte] with     { type C = Int; def apply(a: Char, b: Byte): Int = a + b           }
  given Add[Char, Char] with     { type C = Int; def apply(a: Char, b: Char): Int = a + b           }

  given Sub[Double, Double] with { type C = Double; def apply(a: Double, b: Double): Double = a - b }
  given Sub[Double, Float] with  { type C = Double; def apply(a: Double, b: Float): Double = a - b  }
  given Sub[Double, Long] with   { type C = Double; def apply(a: Double, b: Long): Double = a - b   }
  given Sub[Double, Int] with    { type C = Double; def apply(a: Double, b: Int): Double = a - b    }
  given Sub[Double, Short] with  { type C = Double; def apply(a: Double, b: Short): Double = a - b  }
  given Sub[Double, Byte] with   { type C = Double; def apply(a: Double, b: Byte): Double = a - b   }
  given Sub[Double, Char] with   { type C = Double; def apply(a: Double, b: Char): Double = a - b   }
  given Sub[Float, Double] with  { type C = Double; def apply(a: Float, b: Double): Double = a - b  }
  given Sub[Float, Float] with   { type C = Float; def apply(a: Float, b: Float): Float = a - b     }
  given Sub[Float, Long] with    { type C = Float; def apply(a: Float, b: Long): Float = a - b      }
  given Sub[Float, Int] with     { type C = Float; def apply(a: Float, b: Int): Float = a - b       }
  given Sub[Float, Short] with   { type C = Float; def apply(a: Float, b: Short): Float = a - b     }
  given Sub[Float, Byte] with    { type C = Float; def apply(a: Float, b: Byte): Float = a - b      }
  given Sub[Float, Char] with    { type C = Float; def apply(a: Float, b: Char): Float = a - b      }
  given Sub[Long, Double] with   { type C = Double; def apply(a: Long, b: Double): Double = a - b   }
  given Sub[Long, Float] with    { type C = Float; def apply(a: Long, b: Float): Float = a - b      }
  given Sub[Long, Long] with     { type C = Long; def apply(a: Long, b: Long): Long = a - b         }
  given Sub[Long, Int] with      { type C = Long; def apply(a: Long, b: Int): Long = a - b          }
  given Sub[Long, Short] with    { type C = Long; def apply(a: Long, b: Short): Long = a - b        }
  given Sub[Long, Byte] with     { type C = Long; def apply(a: Long, b: Byte): Long = a - b         }
  given Sub[Long, Char] with     { type C = Long; def apply(a: Long, b: Char): Long = a - b         }
  given Sub[Int, Double] with    { type C = Double; def apply(a: Int, b: Double): Double = a - b    }
  given Sub[Int, Float] with     { type C = Float; def apply(a: Int, b: Float): Float = a - b       }
  given Sub[Int, Long] with      { type C = Long; def apply(a: Int, b: Long): Long = a - b          }
  given Sub[Int, Int] with       { type C = Int; def apply(a: Int, b: Int): Int = a - b             }
  given Sub[Int, Short] with     { type C = Int; def apply(a: Int, b: Short): Int = a - b           }
  given Sub[Int, Byte] with      { type C = Int; def apply(a: Int, b: Byte): Int = a - b            }
  given Sub[Int, Char] with      { type C = Int; def apply(a: Int, b: Char): Int = a - b            }
  given Sub[Short, Double] with  { type C = Double; def apply(a: Short, b: Double): Double = a - b  }
  given Sub[Short, Float] with   { type C = Float; def apply(a: Short, b: Float): Float = a - b     }
  given Sub[Short, Long] with    { type C = Long; def apply(a: Short, b: Long): Long = a - b        }
  given Sub[Short, Int] with     { type C = Int; def apply(a: Short, b: Int): Int = a - b           }
  given Sub[Short, Short] with   { type C = Int; def apply(a: Short, b: Short): Int = a - b         }
  given Sub[Short, Byte] with    { type C = Int; def apply(a: Short, b: Byte): Int = a - b          }
  given Sub[Short, Char] with    { type C = Int; def apply(a: Short, b: Char): Int = a - b          }
  given Sub[Byte, Double] with   { type C = Double; def apply(a: Byte, b: Double): Double = a - b   }
  given Sub[Byte, Float] with    { type C = Float; def apply(a: Byte, b: Float): Float = a - b      }
  given Sub[Byte, Long] with     { type C = Long; def apply(a: Byte, b: Long): Long = a - b         }
  given Sub[Byte, Int] with      { type C = Int; def apply(a: Byte, b: Int): Int = a - b            }
  given Sub[Byte, Short] with    { type C = Int; def apply(a: Byte, b: Short): Int = a - b          }
  given Sub[Byte, Byte] with     { type C = Int; def apply(a: Byte, b: Byte): Int = a - b           }
  given Sub[Byte, Char] with     { type C = Int; def apply(a: Byte, b: Char): Int = a - b           }
  given Sub[Char, Double] with   { type C = Double; def apply(a: Char, b: Double): Double = a - b   }
  given Sub[Char, Float] with    { type C = Float; def apply(a: Char, b: Float): Float = a - b      }
  given Sub[Char, Long] with     { type C = Long; def apply(a: Char, b: Long): Long = a - b         }
  given Sub[Char, Int] with      { type C = Int; def apply(a: Char, b: Int): Int = a - b            }
  given Sub[Char, Short] with    { type C = Int; def apply(a: Char, b: Short): Int = a - b          }
  given Sub[Char, Byte] with     { type C = Int; def apply(a: Char, b: Byte): Int = a - b           }
  given Sub[Char, Char] with     { type C = Int; def apply(a: Char, b: Char): Int = a - b           }

  given Times[Double, Double] with { type C = Double; def apply(a: Double, b: Double): Double = a * b }
  given Times[Double, Float] with  { type C = Double; def apply(a: Double, b: Float): Double = a * b  }
  given Times[Double, Long] with   { type C = Double; def apply(a: Double, b: Long): Double = a * b   }
  given Times[Double, Int] with    { type C = Double; def apply(a: Double, b: Int): Double = a * b    }
  given Times[Double, Short] with  { type C = Double; def apply(a: Double, b: Short): Double = a * b  }
  given Times[Double, Byte] with   { type C = Double; def apply(a: Double, b: Byte): Double = a * b   }
  given Times[Double, Char] with   { type C = Double; def apply(a: Double, b: Char): Double = a * b   }
  given Times[Float, Double] with  { type C = Double; def apply(a: Float, b: Double): Double = a * b  }
  given Times[Float, Float] with   { type C = Float; def apply(a: Float, b: Float): Float = a * b     }
  given Times[Float, Long] with    { type C = Float; def apply(a: Float, b: Long): Float = a * b      }
  given Times[Float, Int] with     { type C = Float; def apply(a: Float, b: Int): Float = a * b       }
  given Times[Float, Short] with   { type C = Float; def apply(a: Float, b: Short): Float = a * b     }
  given Times[Float, Byte] with    { type C = Float; def apply(a: Float, b: Byte): Float = a * b      }
  given Times[Float, Char] with    { type C = Float; def apply(a: Float, b: Char): Float = a * b      }
  given Times[Long, Double] with   { type C = Double; def apply(a: Long, b: Double): Double = a * b   }
  given Times[Long, Float] with    { type C = Float; def apply(a: Long, b: Float): Float = a * b      }
  given Times[Long, Long] with     { type C = Long; def apply(a: Long, b: Long): Long = a * b         }
  given Times[Long, Int] with      { type C = Long; def apply(a: Long, b: Int): Long = a * b          }
  given Times[Long, Short] with    { type C = Long; def apply(a: Long, b: Short): Long = a * b        }
  given Times[Long, Byte] with     { type C = Long; def apply(a: Long, b: Byte): Long = a * b         }
  given Times[Long, Char] with     { type C = Long; def apply(a: Long, b: Char): Long = a * b         }
  given Times[Int, Double] with    { type C = Double; def apply(a: Int, b: Double): Double = a * b    }
  given Times[Int, Float] with     { type C = Float; def apply(a: Int, b: Float): Float = a * b       }
  given Times[Int, Long] with      { type C = Long; def apply(a: Int, b: Long): Long = a * b          }
  given Times[Int, Int] with       { type C = Int; def apply(a: Int, b: Int): Int = a * b             }
  given Times[Int, Short] with     { type C = Int; def apply(a: Int, b: Short): Int = a * b           }
  given Times[Int, Byte] with      { type C = Int; def apply(a: Int, b: Byte): Int = a * b            }
  given Times[Int, Char] with      { type C = Int; def apply(a: Int, b: Char): Int = a * b            }
  given Times[Short, Double] with  { type C = Double; def apply(a: Short, b: Double): Double = a * b  }
  given Times[Short, Float] with   { type C = Float; def apply(a: Short, b: Float): Float = a * b     }
  given Times[Short, Long] with    { type C = Long; def apply(a: Short, b: Long): Long = a * b        }
  given Times[Short, Int] with     { type C = Int; def apply(a: Short, b: Int): Int = a * b           }
  given Times[Short, Short] with   { type C = Int; def apply(a: Short, b: Short): Int = a * b         }
  given Times[Short, Byte] with    { type C = Int; def apply(a: Short, b: Byte): Int = a * b          }
  given Times[Short, Char] with    { type C = Int; def apply(a: Short, b: Char): Int = a * b          }
  given Times[Byte, Double] with   { type C = Double; def apply(a: Byte, b: Double): Double = a * b   }
  given Times[Byte, Float] with    { type C = Float; def apply(a: Byte, b: Float): Float = a * b      }
  given Times[Byte, Long] with     { type C = Long; def apply(a: Byte, b: Long): Long = a * b         }
  given Times[Byte, Int] with      { type C = Int; def apply(a: Byte, b: Int): Int = a * b            }
  given Times[Byte, Short] with    { type C = Int; def apply(a: Byte, b: Short): Int = a * b          }
  given Times[Byte, Byte] with     { type C = Int; def apply(a: Byte, b: Byte): Int = a * b           }
  given Times[Byte, Char] with     { type C = Int; def apply(a: Byte, b: Char): Int = a * b           }
  given Times[Char, Double] with   { type C = Double; def apply(a: Char, b: Double): Double = a * b   }
  given Times[Char, Float] with    { type C = Float; def apply(a: Char, b: Float): Float = a * b      }
  given Times[Char, Long] with     { type C = Long; def apply(a: Char, b: Long): Long = a * b         }
  given Times[Char, Int] with      { type C = Int; def apply(a: Char, b: Int): Int = a * b            }
  given Times[Char, Short] with    { type C = Int; def apply(a: Char, b: Short): Int = a * b          }
  given Times[Char, Byte] with     { type C = Int; def apply(a: Char, b: Byte): Int = a * b           }
  given Times[Char, Char] with     { type C = Int; def apply(a: Char, b: Char): Int = a * b           }

  given Div[Double, Double] with { type C = Double; def apply(a: Double, b: Double): Double = a / b }
  given Div[Double, Float] with  { type C = Double; def apply(a: Double, b: Float): Double = a / b  }
  given Div[Double, Long] with   { type C = Double; def apply(a: Double, b: Long): Double = a / b   }
  given Div[Double, Int] with    { type C = Double; def apply(a: Double, b: Int): Double = a / b    }
  given Div[Double, Short] with  { type C = Double; def apply(a: Double, b: Short): Double = a / b  }
  given Div[Double, Byte] with   { type C = Double; def apply(a: Double, b: Byte): Double = a / b   }
  given Div[Double, Char] with   { type C = Double; def apply(a: Double, b: Char): Double = a / b   }
  given Div[Float, Double] with  { type C = Double; def apply(a: Float, b: Double): Double = a / b  }
  given Div[Float, Float] with   { type C = Float; def apply(a: Float, b: Float): Float = a / b     }
  given Div[Float, Long] with    { type C = Float; def apply(a: Float, b: Long): Float = a / b      }
  given Div[Float, Int] with     { type C = Float; def apply(a: Float, b: Int): Float = a / b       }
  given Div[Float, Short] with   { type C = Float; def apply(a: Float, b: Short): Float = a / b     }
  given Div[Float, Byte] with    { type C = Float; def apply(a: Float, b: Byte): Float = a / b      }
  given Div[Float, Char] with    { type C = Float; def apply(a: Float, b: Char): Float = a / b      }
  given Div[Long, Double] with   { type C = Double; def apply(a: Long, b: Double): Double = a / b   }
  given Div[Long, Float] with    { type C = Float; def apply(a: Long, b: Float): Float = a / b      }
  given Div[Long, Long] with     { type C = Long; def apply(a: Long, b: Long): Long = a / b         }
  given Div[Long, Int] with      { type C = Long; def apply(a: Long, b: Int): Long = a / b          }
  given Div[Long, Short] with    { type C = Long; def apply(a: Long, b: Short): Long = a / b        }
  given Div[Long, Byte] with     { type C = Long; def apply(a: Long, b: Byte): Long = a / b         }
  given Div[Long, Char] with     { type C = Long; def apply(a: Long, b: Char): Long = a / b         }
  given Div[Int, Double] with    { type C = Double; def apply(a: Int, b: Double): Double = a / b    }
  given Div[Int, Float] with     { type C = Float; def apply(a: Int, b: Float): Float = a / b       }
  given Div[Int, Long] with      { type C = Long; def apply(a: Int, b: Long): Long = a / b          }
  given Div[Int, Int] with       { type C = Int; def apply(a: Int, b: Int): Int = a / b             }
  given Div[Int, Short] with     { type C = Int; def apply(a: Int, b: Short): Int = a / b           }
  given Div[Int, Byte] with      { type C = Int; def apply(a: Int, b: Byte): Int = a / b            }
  given Div[Int, Char] with      { type C = Int; def apply(a: Int, b: Char): Int = a / b            }
  given Div[Short, Double] with  { type C = Double; def apply(a: Short, b: Double): Double = a / b  }
  given Div[Short, Float] with   { type C = Float; def apply(a: Short, b: Float): Float = a / b     }
  given Div[Short, Long] with    { type C = Long; def apply(a: Short, b: Long): Long = a / b        }
  given Div[Short, Int] with     { type C = Int; def apply(a: Short, b: Int): Int = a / b           }
  given Div[Short, Short] with   { type C = Int; def apply(a: Short, b: Short): Int = a / b         }
  given Div[Short, Byte] with    { type C = Int; def apply(a: Short, b: Byte): Int = a / b          }
  given Div[Short, Char] with    { type C = Int; def apply(a: Short, b: Char): Int = a / b          }
  given Div[Byte, Double] with   { type C = Double; def apply(a: Byte, b: Double): Double = a / b   }
  given Div[Byte, Float] with    { type C = Float; def apply(a: Byte, b: Float): Float = a / b      }
  given Div[Byte, Long] with     { type C = Long; def apply(a: Byte, b: Long): Long = a / b         }
  given Div[Byte, Int] with      { type C = Int; def apply(a: Byte, b: Int): Int = a / b            }
  given Div[Byte, Short] with    { type C = Int; def apply(a: Byte, b: Short): Int = a / b          }
  given Div[Byte, Byte] with     { type C = Int; def apply(a: Byte, b: Byte): Int = a / b           }
  given Div[Byte, Char] with     { type C = Int; def apply(a: Byte, b: Char): Int = a / b           }
  given Div[Char, Double] with   { type C = Double; def apply(a: Char, b: Double): Double = a / b   }
  given Div[Char, Float] with    { type C = Float; def apply(a: Char, b: Float): Float = a / b      }
  given Div[Char, Long] with     { type C = Long; def apply(a: Char, b: Long): Long = a / b         }
  given Div[Char, Int] with      { type C = Int; def apply(a: Char, b: Int): Int = a / b            }
  given Div[Char, Short] with    { type C = Int; def apply(a: Char, b: Short): Int = a / b          }
  given Div[Char, Byte] with     { type C = Int; def apply(a: Char, b: Byte): Int = a / b           }
  given Div[Char, Char] with     { type C = Int; def apply(a: Char, b: Char): Int = a / b           }

  given Rem[Double, Double] with { type C = Double; def apply(a: Double, b: Double): Double = a % b }
  given Rem[Double, Float] with  { type C = Double; def apply(a: Double, b: Float): Double = a % b  }
  given Rem[Double, Long] with   { type C = Double; def apply(a: Double, b: Long): Double = a % b   }
  given Rem[Double, Int] with    { type C = Double; def apply(a: Double, b: Int): Double = a % b    }
  given Rem[Double, Short] with  { type C = Double; def apply(a: Double, b: Short): Double = a % b  }
  given Rem[Double, Byte] with   { type C = Double; def apply(a: Double, b: Byte): Double = a % b   }
  given Rem[Double, Char] with   { type C = Double; def apply(a: Double, b: Char): Double = a % b   }
  given Rem[Float, Double] with  { type C = Double; def apply(a: Float, b: Double): Double = a % b  }
  given Rem[Float, Float] with   { type C = Float; def apply(a: Float, b: Float): Float = a % b     }
  given Rem[Float, Long] with    { type C = Float; def apply(a: Float, b: Long): Float = a % b      }
  given Rem[Float, Int] with     { type C = Float; def apply(a: Float, b: Int): Float = a % b       }
  given Rem[Float, Short] with   { type C = Float; def apply(a: Float, b: Short): Float = a % b     }
  given Rem[Float, Byte] with    { type C = Float; def apply(a: Float, b: Byte): Float = a % b      }
  given Rem[Float, Char] with    { type C = Float; def apply(a: Float, b: Char): Float = a % b      }
  given Rem[Long, Double] with   { type C = Double; def apply(a: Long, b: Double): Double = a % b   }
  given Rem[Long, Float] with    { type C = Float; def apply(a: Long, b: Float): Float = a % b      }
  given Rem[Long, Long] with     { type C = Long; def apply(a: Long, b: Long): Long = a % b         }
  given Rem[Long, Int] with      { type C = Long; def apply(a: Long, b: Int): Long = a % b          }
  given Rem[Long, Short] with    { type C = Long; def apply(a: Long, b: Short): Long = a % b        }
  given Rem[Long, Byte] with     { type C = Long; def apply(a: Long, b: Byte): Long = a % b         }
  given Rem[Long, Char] with     { type C = Long; def apply(a: Long, b: Char): Long = a % b         }
  given Rem[Int, Double] with    { type C = Double; def apply(a: Int, b: Double): Double = a % b    }
  given Rem[Int, Float] with     { type C = Float; def apply(a: Int, b: Float): Float = a % b       }
  given Rem[Int, Long] with      { type C = Long; def apply(a: Int, b: Long): Long = a % b          }
  given Rem[Int, Int] with       { type C = Int; def apply(a: Int, b: Int): Int = a % b             }
  given Rem[Int, Short] with     { type C = Int; def apply(a: Int, b: Short): Int = a % b           }
  given Rem[Int, Byte] with      { type C = Int; def apply(a: Int, b: Byte): Int = a % b            }
  given Rem[Int, Char] with      { type C = Int; def apply(a: Int, b: Char): Int = a % b            }
  given Rem[Short, Double] with  { type C = Double; def apply(a: Short, b: Double): Double = a % b  }
  given Rem[Short, Float] with   { type C = Float; def apply(a: Short, b: Float): Float = a % b     }
  given Rem[Short, Long] with    { type C = Long; def apply(a: Short, b: Long): Long = a % b        }
  given Rem[Short, Int] with     { type C = Int; def apply(a: Short, b: Int): Int = a % b           }
  given Rem[Short, Short] with   { type C = Int; def apply(a: Short, b: Short): Int = a % b         }
  given Rem[Short, Byte] with    { type C = Int; def apply(a: Short, b: Byte): Int = a % b          }
  given Rem[Short, Char] with    { type C = Int; def apply(a: Short, b: Char): Int = a % b          }
  given Rem[Byte, Double] with   { type C = Double; def apply(a: Byte, b: Double): Double = a % b   }
  given Rem[Byte, Float] with    { type C = Float; def apply(a: Byte, b: Float): Float = a % b      }
  given Rem[Byte, Long] with     { type C = Long; def apply(a: Byte, b: Long): Long = a % b         }
  given Rem[Byte, Int] with      { type C = Int; def apply(a: Byte, b: Int): Int = a % b            }
  given Rem[Byte, Short] with    { type C = Int; def apply(a: Byte, b: Short): Int = a % b          }
  given Rem[Byte, Byte] with     { type C = Int; def apply(a: Byte, b: Byte): Int = a % b           }
  given Rem[Byte, Char] with     { type C = Int; def apply(a: Byte, b: Char): Int = a % b           }
  given Rem[Char, Double] with   { type C = Double; def apply(a: Char, b: Double): Double = a % b   }
  given Rem[Char, Float] with    { type C = Float; def apply(a: Char, b: Float): Float = a % b      }
  given Rem[Char, Long] with     { type C = Long; def apply(a: Char, b: Long): Long = a % b         }
  given Rem[Char, Int] with      { type C = Int; def apply(a: Char, b: Int): Int = a % b            }
  given Rem[Char, Short] with    { type C = Int; def apply(a: Char, b: Short): Int = a % b          }
  given Rem[Char, Byte] with     { type C = Int; def apply(a: Char, b: Byte): Int = a % b           }
  given Rem[Char, Char] with     { type C = Int; def apply(a: Char, b: Char): Int = a % b           }

  opaque type ScalarOnLhsWitness          = Unit
  opaque type ScalarOnRhsWitness          = Unit
  opaque type UncertainOnBothSidesWitness = Unit
  given ScalarOnLhsWitness          = ()
  given ScalarOnRhsWitness          = ()
  given UncertainOnBothSidesWitness = ()

  // Extensions for Uncertain on the left
  extension [A](lhs: Uncertain[A]) {
    // This matches when rhs is NOT Uncertain
    def +[B](rhs: B)(using op: Add[A, B], witness: ScalarOnRhsWitness): Uncertain[op.C] =
      lhs.map(a => op(a, rhs))

    def -[B](rhs: B)(using op: Sub[A, B], witness: ScalarOnRhsWitness): Uncertain[op.C] =
      lhs.map(a => op(a, rhs))

    def *[B](rhs: B)(using op: Times[A, B], witness: ScalarOnRhsWitness): Uncertain[op.C] =
      lhs.map(a => op(a, rhs))

    def /[B](rhs: B)(using op: Div[A, B], witness: ScalarOnRhsWitness): Uncertain[op.C] =
      lhs.map(a => op(a, rhs))

    def %[B](rhs: B)(using op: Rem[A, B], witness: ScalarOnRhsWitness): Uncertain[op.C] =
      lhs.map(a => op(a, rhs))

    // Specific overloads for Uncertain + Uncertain
    def +[B](rhs: Uncertain[B])(using op: Add[A, B], witness: UncertainOnBothSidesWitness): Uncertain[op.C] =
      lhs.zipWith(rhs)(op.apply)

    def -[B](rhs: Uncertain[B])(using op: Sub[A, B], witness: UncertainOnBothSidesWitness): Uncertain[op.C] =
      lhs.zipWith(rhs)(op.apply)

    def *[B](rhs: Uncertain[B])(using op: Times[A, B], witness: UncertainOnBothSidesWitness): Uncertain[op.C] =
      lhs.zipWith(rhs)(op.apply)

    def /[B](rhs: Uncertain[B])(using op: Div[A, B], witness: UncertainOnBothSidesWitness): Uncertain[op.C] =
      lhs.zipWith(rhs)(op.apply)

    def %[B](rhs: Uncertain[B])(using op: Rem[A, B], witness: UncertainOnBothSidesWitness): Uncertain[op.C] =
      lhs.zipWith(rhs)(op.apply)
  }

//TODO - I can't get this to compile
//  extension [A](lhs: A) {
//    def +[B](rhs: Uncertain[B])(using op: Add[A, B], witness: ScalarOnLhsWitness, ev: NotGiven[A <:< Uncertain[?]]): Uncertain[op.C] =
//      rhs.map(op(lhs,_))
//
//    def -[B](rhs: Uncertain[B])(using op: Sub[A, B], witness: ScalarOnLhsWitness): Uncertain[op.C] =
//      rhs.map(op(lhs,_))
//
//    def *[B](rhs: Uncertain[B])(using op: Times[A, B], witness: ScalarOnLhsWitness): Uncertain[op.C] =
//      rhs.map(op(lhs, _))
//
//    def /[B](rhs: Uncertain[B])(using op: Div[A, B], witness: ScalarOnLhsWitness): Uncertain[op.C] =
//      rhs.map(op(lhs, _))
//
//    def %[B](rhs: Uncertain[B])(using op: Rem[A, B], witness: ScalarOnLhsWitness): Uncertain[op.C] =
//      rhs.map(op(lhs, _))
//  }

}
