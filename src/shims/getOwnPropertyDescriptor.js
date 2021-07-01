
module.exports = function( object, key ) {
  
    var hasSupport =
      typeof object.__lookupGetter__ === 'function' &&
      typeof object.__lookupSetter__ === 'function'
    
    // TODO: How does one determine this?!
    var isGetterSetter = !hasSupport ? null :
      object.__lookupGetter__( key ) ||
      object.__lookupSetter__( key )
    
    return isGetterSetter != null ? {
      configurable: true,
      enumerable: true,
      get: object.__lookupGetter__( key ),
      set: object.__lookupSetter__( key )
    } : {
      configurable: true,
      writable: true,
      enumerable: true,
      value: object[ key ]
    }
    
  }