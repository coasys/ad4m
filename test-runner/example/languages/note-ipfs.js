'use strict';

Object.defineProperty(exports, '__esModule', { value: true });

var __classPrivateFieldSet$1 = (undefined && undefined.__classPrivateFieldSet) || function (receiver, privateMap, value) {
    if (!privateMap.has(receiver)) {
        throw new TypeError("attempted to set private field on non-instance");
    }
    privateMap.set(receiver, value);
    return value;
};
var __classPrivateFieldGet$1 = (undefined && undefined.__classPrivateFieldGet) || function (receiver, privateMap) {
    if (!privateMap.has(receiver)) {
        throw new TypeError("attempted to get private field on non-instance");
    }
    return privateMap.get(receiver);
};
var _agent, _IPFS$1;
class IpfsPutAdapter {
    constructor(context) {
        _agent.set(this, void 0);
        _IPFS$1.set(this, void 0);
        __classPrivateFieldSet$1(this, _agent, context.agent);
        __classPrivateFieldSet$1(this, _IPFS$1, context.IPFS);
    }
    async createPublic(note) {
        try {
            //@ts-ignore
            note = JSON.parse(note);
        }
        catch (e) {
        }
        const agent = __classPrivateFieldGet$1(this, _agent);
        const expression = agent.createSignedExpression(note);
        const content = JSON.stringify(expression);
        const result = await __classPrivateFieldGet$1(this, _IPFS$1).add({ content });
        // @ts-ignore
        return result.cid.toString();
    }
}
_agent = new WeakMap(), _IPFS$1 = new WeakMap();

function createCommonjsModule(fn, basedir, module) {
	return module = {
	  path: basedir,
	  exports: {},
	  require: function (path, base) {
      return commonjsRequire(path, (base === undefined || base === null) ? module.path : base);
    }
	}, fn(module, module.exports), module.exports;
}

function commonjsRequire () {
	throw new Error('Dynamic requires are not currently supported by @rollup/plugin-commonjs');
}

function base$1(ALPHABET, name) {
  if (ALPHABET.length >= 255) {
    throw new TypeError('Alphabet too long');
  }
  var BASE_MAP = new Uint8Array(256);
  for (var j = 0; j < BASE_MAP.length; j++) {
    BASE_MAP[j] = 255;
  }
  for (var i = 0; i < ALPHABET.length; i++) {
    var x = ALPHABET.charAt(i);
    var xc = x.charCodeAt(0);
    if (BASE_MAP[xc] !== 255) {
      throw new TypeError(x + ' is ambiguous');
    }
    BASE_MAP[xc] = i;
  }
  var BASE = ALPHABET.length;
  var LEADER = ALPHABET.charAt(0);
  var FACTOR = Math.log(BASE) / Math.log(256);
  var iFACTOR = Math.log(256) / Math.log(BASE);
  function encode(source) {
    if (source instanceof Uint8Array);
    else if (ArrayBuffer.isView(source)) {
      source = new Uint8Array(source.buffer, source.byteOffset, source.byteLength);
    } else if (Array.isArray(source)) {
      source = Uint8Array.from(source);
    }
    if (!(source instanceof Uint8Array)) {
      throw new TypeError('Expected Uint8Array');
    }
    if (source.length === 0) {
      return '';
    }
    var zeroes = 0;
    var length = 0;
    var pbegin = 0;
    var pend = source.length;
    while (pbegin !== pend && source[pbegin] === 0) {
      pbegin++;
      zeroes++;
    }
    var size = (pend - pbegin) * iFACTOR + 1 >>> 0;
    var b58 = new Uint8Array(size);
    while (pbegin !== pend) {
      var carry = source[pbegin];
      var i = 0;
      for (var it1 = size - 1; (carry !== 0 || i < length) && it1 !== -1; it1--, i++) {
        carry += 256 * b58[it1] >>> 0;
        b58[it1] = carry % BASE >>> 0;
        carry = carry / BASE >>> 0;
      }
      if (carry !== 0) {
        throw new Error('Non-zero carry');
      }
      length = i;
      pbegin++;
    }
    var it2 = size - length;
    while (it2 !== size && b58[it2] === 0) {
      it2++;
    }
    var str = LEADER.repeat(zeroes);
    for (; it2 < size; ++it2) {
      str += ALPHABET.charAt(b58[it2]);
    }
    return str;
  }
  function decodeUnsafe(source) {
    if (typeof source !== 'string') {
      throw new TypeError('Expected String');
    }
    if (source.length === 0) {
      return new Uint8Array();
    }
    var psz = 0;
    if (source[psz] === ' ') {
      return;
    }
    var zeroes = 0;
    var length = 0;
    while (source[psz] === LEADER) {
      zeroes++;
      psz++;
    }
    var size = (source.length - psz) * FACTOR + 1 >>> 0;
    var b256 = new Uint8Array(size);
    while (source[psz]) {
      var carry = BASE_MAP[source.charCodeAt(psz)];
      if (carry === 255) {
        return;
      }
      var i = 0;
      for (var it3 = size - 1; (carry !== 0 || i < length) && it3 !== -1; it3--, i++) {
        carry += BASE * b256[it3] >>> 0;
        b256[it3] = carry % 256 >>> 0;
        carry = carry / 256 >>> 0;
      }
      if (carry !== 0) {
        throw new Error('Non-zero carry');
      }
      length = i;
      psz++;
    }
    if (source[psz] === ' ') {
      return;
    }
    var it4 = size - length;
    while (it4 !== size && b256[it4] === 0) {
      it4++;
    }
    var vch = new Uint8Array(zeroes + (size - it4));
    var j = zeroes;
    while (it4 !== size) {
      vch[j++] = b256[it4++];
    }
    return vch;
  }
  function decode(string) {
    var buffer = decodeUnsafe(string);
    if (buffer) {
      return buffer;
    }
    throw new Error(`Non-${ name } character`);
  }
  return {
    encode: encode,
    decodeUnsafe: decodeUnsafe,
    decode: decode
  };
}
var src = base$1;
var _brrp__multiformats_scope_baseX = src;

var baseX = _brrp__multiformats_scope_baseX;

var bytes = createCommonjsModule(function (module, exports) {

Object.defineProperty(exports, '__esModule', { value: true });

const empty = new Uint8Array(0);
const toHex = d => d.reduce((hex, byte) => hex + byte.toString(16).padStart(2, '0'), '');
const fromHex = hex => {
  const hexes = hex.match(/../g);
  return hexes ? new Uint8Array(hexes.map(b => parseInt(b, 16))) : empty;
};
const equals = (aa, bb) => {
  if (aa === bb)
    return true;
  if (aa.byteLength !== bb.byteLength) {
    return false;
  }
  for (let ii = 0; ii < aa.byteLength; ii++) {
    if (aa[ii] !== bb[ii]) {
      return false;
    }
  }
  return true;
};
const coerce = o => {
  if (o instanceof Uint8Array && o.constructor.name === 'Uint8Array')
    return o;
  if (o instanceof ArrayBuffer)
    return new Uint8Array(o);
  if (ArrayBuffer.isView(o)) {
    return new Uint8Array(o.buffer, o.byteOffset, o.byteLength);
  }
  throw new Error('Unknown type, must be binary type');
};
const isBinary = o => o instanceof ArrayBuffer || ArrayBuffer.isView(o);
const fromString = str => new TextEncoder().encode(str);
const toString = b => new TextDecoder().decode(b);

exports.coerce = coerce;
exports.empty = empty;
exports.equals = equals;
exports.fromHex = fromHex;
exports.fromString = fromString;
exports.isBinary = isBinary;
exports.toHex = toHex;
exports.toString = toString;
});

var base = createCommonjsModule(function (module, exports) {

Object.defineProperty(exports, '__esModule', { value: true });




class Encoder {
  constructor(name, prefix, baseEncode) {
    this.name = name;
    this.prefix = prefix;
    this.baseEncode = baseEncode;
  }
  encode(bytes) {
    if (bytes instanceof Uint8Array) {
      return `${ this.prefix }${ this.baseEncode(bytes) }`;
    } else {
      throw Error('Unknown type, must be binary type');
    }
  }
}
class Decoder {
  constructor(name, prefix, baseDecode) {
    this.name = name;
    this.prefix = prefix;
    this.baseDecode = baseDecode;
  }
  decode(text) {
    if (typeof text === 'string') {
      switch (text[0]) {
      case this.prefix: {
          return this.baseDecode(text.slice(1));
        }
      default: {
          throw Error(`Unable to decode multibase string ${ JSON.stringify(text) }, ${ this.name } decoder only supports inputs prefixed with ${ this.prefix }`);
        }
      }
    } else {
      throw Error('Can only multibase decode strings');
    }
  }
  or(decoder) {
    return or(this, decoder);
  }
}
class ComposedDecoder {
  constructor(decoders) {
    this.decoders = decoders;
  }
  or(decoder) {
    return or(this, decoder);
  }
  decode(input) {
    const prefix = input[0];
    const decoder = this.decoders[prefix];
    if (decoder) {
      return decoder.decode(input);
    } else {
      throw RangeError(`Unable to decode multibase string ${ JSON.stringify(input) }, only inputs prefixed with ${ Object.keys(this.decoders) } are supported`);
    }
  }
}
const or = (left, right) => new ComposedDecoder({
  ...left.decoders || { [left.prefix]: left },
  ...right.decoders || { [right.prefix]: right }
});
class Codec {
  constructor(name, prefix, baseEncode, baseDecode) {
    this.name = name;
    this.prefix = prefix;
    this.baseEncode = baseEncode;
    this.baseDecode = baseDecode;
    this.encoder = new Encoder(name, prefix, baseEncode);
    this.decoder = new Decoder(name, prefix, baseDecode);
  }
  encode(input) {
    return this.encoder.encode(input);
  }
  decode(input) {
    return this.decoder.decode(input);
  }
}
const from = ({name, prefix, encode, decode}) => new Codec(name, prefix, encode, decode);
const baseX$1 = ({prefix, name, alphabet}) => {
  const {encode, decode} = baseX(alphabet, name);
  return from({
    prefix,
    name,
    encode,
    decode: text => bytes.coerce(decode(text))
  });
};
const decode = (string, alphabet, bitsPerChar, name) => {
  const codes = {};
  for (let i = 0; i < alphabet.length; ++i) {
    codes[alphabet[i]] = i;
  }
  let end = string.length;
  while (string[end - 1] === '=') {
    --end;
  }
  const out = new Uint8Array(end * bitsPerChar / 8 | 0);
  let bits = 0;
  let buffer = 0;
  let written = 0;
  for (let i = 0; i < end; ++i) {
    const value = codes[string[i]];
    if (value === undefined) {
      throw new SyntaxError(`Non-${ name } character`);
    }
    buffer = buffer << bitsPerChar | value;
    bits += bitsPerChar;
    if (bits >= 8) {
      bits -= 8;
      out[written++] = 255 & buffer >> bits;
    }
  }
  if (bits >= bitsPerChar || 255 & buffer << 8 - bits) {
    throw new SyntaxError('Unexpected end of data');
  }
  return out;
};
const encode = (data, alphabet, bitsPerChar) => {
  const pad = alphabet[alphabet.length - 1] === '=';
  const mask = (1 << bitsPerChar) - 1;
  let out = '';
  let bits = 0;
  let buffer = 0;
  for (let i = 0; i < data.length; ++i) {
    buffer = buffer << 8 | data[i];
    bits += 8;
    while (bits > bitsPerChar) {
      bits -= bitsPerChar;
      out += alphabet[mask & buffer >> bits];
    }
  }
  if (bits) {
    out += alphabet[mask & buffer << bitsPerChar - bits];
  }
  if (pad) {
    while (out.length * bitsPerChar & 7) {
      out += '=';
    }
  }
  return out;
};
const rfc4648 = ({name, prefix, bitsPerChar, alphabet}) => {
  return from({
    prefix,
    name,
    encode(input) {
      return encode(input, alphabet, bitsPerChar);
    },
    decode(input) {
      return decode(input, alphabet, bitsPerChar, name);
    }
  });
};

exports.Codec = Codec;
exports.baseX = baseX$1;
exports.from = from;
exports.or = or;
exports.rfc4648 = rfc4648;
});

var identity_1$1 = createCommonjsModule(function (module, exports) {

Object.defineProperty(exports, '__esModule', { value: true });




const identity = base.from({
  prefix: '\0',
  name: 'identity',
  encode: buf => bytes.toString(buf),
  decode: str => bytes.fromString(str)
});

exports.identity = identity;
});

var base2_1 = createCommonjsModule(function (module, exports) {

Object.defineProperty(exports, '__esModule', { value: true });



const base2 = base.rfc4648({
  prefix: '0',
  name: 'base2',
  alphabet: '01',
  bitsPerChar: 1
});

exports.base2 = base2;
});

var base8_1 = createCommonjsModule(function (module, exports) {

Object.defineProperty(exports, '__esModule', { value: true });



const base8 = base.rfc4648({
  prefix: '7',
  name: 'base8',
  alphabet: '01234567',
  bitsPerChar: 3
});

exports.base8 = base8;
});

var base10_1 = createCommonjsModule(function (module, exports) {

Object.defineProperty(exports, '__esModule', { value: true });



const base10 = base.baseX({
  prefix: '9',
  name: 'base10',
  alphabet: '0123456789'
});

exports.base10 = base10;
});

var base16_1 = createCommonjsModule(function (module, exports) {

Object.defineProperty(exports, '__esModule', { value: true });



const base16 = base.rfc4648({
  prefix: 'f',
  name: 'base16',
  alphabet: '0123456789abcdef',
  bitsPerChar: 4
});
const base16upper = base.rfc4648({
  prefix: 'F',
  name: 'base16upper',
  alphabet: '0123456789ABCDEF',
  bitsPerChar: 4
});

exports.base16 = base16;
exports.base16upper = base16upper;
});

var base32_1 = createCommonjsModule(function (module, exports) {

Object.defineProperty(exports, '__esModule', { value: true });



const base32 = base.rfc4648({
  prefix: 'b',
  name: 'base32',
  alphabet: 'abcdefghijklmnopqrstuvwxyz234567',
  bitsPerChar: 5
});
const base32upper = base.rfc4648({
  prefix: 'B',
  name: 'base32upper',
  alphabet: 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567',
  bitsPerChar: 5
});
const base32pad = base.rfc4648({
  prefix: 'c',
  name: 'base32pad',
  alphabet: 'abcdefghijklmnopqrstuvwxyz234567=',
  bitsPerChar: 5
});
const base32padupper = base.rfc4648({
  prefix: 'C',
  name: 'base32padupper',
  alphabet: 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567=',
  bitsPerChar: 5
});
const base32hex = base.rfc4648({
  prefix: 'v',
  name: 'base32hex',
  alphabet: '0123456789abcdefghijklmnopqrstuv',
  bitsPerChar: 5
});
const base32hexupper = base.rfc4648({
  prefix: 'V',
  name: 'base32hexupper',
  alphabet: '0123456789ABCDEFGHIJKLMNOPQRSTUV',
  bitsPerChar: 5
});
const base32hexpad = base.rfc4648({
  prefix: 't',
  name: 'base32hexpad',
  alphabet: '0123456789abcdefghijklmnopqrstuv=',
  bitsPerChar: 5
});
const base32hexpadupper = base.rfc4648({
  prefix: 'T',
  name: 'base32hexpadupper',
  alphabet: '0123456789ABCDEFGHIJKLMNOPQRSTUV=',
  bitsPerChar: 5
});
const base32z = base.rfc4648({
  prefix: 'h',
  name: 'base32z',
  alphabet: 'ybndrfg8ejkmcpqxot1uwisza345h769',
  bitsPerChar: 5
});

exports.base32 = base32;
exports.base32hex = base32hex;
exports.base32hexpad = base32hexpad;
exports.base32hexpadupper = base32hexpadupper;
exports.base32hexupper = base32hexupper;
exports.base32pad = base32pad;
exports.base32padupper = base32padupper;
exports.base32upper = base32upper;
exports.base32z = base32z;
});

var base36_1 = createCommonjsModule(function (module, exports) {

Object.defineProperty(exports, '__esModule', { value: true });



const base36 = base.baseX({
  prefix: 'k',
  name: 'base36',
  alphabet: '0123456789abcdefghijklmnopqrstuvwxyz'
});
const base36upper = base.baseX({
  prefix: 'K',
  name: 'base36upper',
  alphabet: '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'
});

exports.base36 = base36;
exports.base36upper = base36upper;
});

var base58 = createCommonjsModule(function (module, exports) {

Object.defineProperty(exports, '__esModule', { value: true });



const base58btc = base.baseX({
  name: 'base58btc',
  prefix: 'z',
  alphabet: '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'
});
const base58flickr = base.baseX({
  name: 'base58flickr',
  prefix: 'Z',
  alphabet: '123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ'
});

exports.base58btc = base58btc;
exports.base58flickr = base58flickr;
});

var base64_1 = createCommonjsModule(function (module, exports) {

Object.defineProperty(exports, '__esModule', { value: true });



const base64 = base.rfc4648({
  prefix: 'm',
  name: 'base64',
  alphabet: 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/',
  bitsPerChar: 6
});
const base64pad = base.rfc4648({
  prefix: 'M',
  name: 'base64pad',
  alphabet: 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=',
  bitsPerChar: 6
});
const base64url = base.rfc4648({
  prefix: 'u',
  name: 'base64url',
  alphabet: 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_',
  bitsPerChar: 6
});
const base64urlpad = base.rfc4648({
  prefix: 'U',
  name: 'base64urlpad',
  alphabet: 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_=',
  bitsPerChar: 6
});

exports.base64 = base64;
exports.base64pad = base64pad;
exports.base64url = base64url;
exports.base64urlpad = base64urlpad;
});

var encode_1 = encode;
var MSB = 128, REST = 127, MSBALL = ~REST, INT = Math.pow(2, 31);
function encode(num, out, offset) {
  out = out || [];
  offset = offset || 0;
  var oldOffset = offset;
  while (num >= INT) {
    out[offset++] = num & 255 | MSB;
    num /= 128;
  }
  while (num & MSBALL) {
    out[offset++] = num & 255 | MSB;
    num >>>= 7;
  }
  out[offset] = num | 0;
  encode.bytes = offset - oldOffset + 1;
  return out;
}
var decode = read;
var MSB$1 = 128, REST$1 = 127;
function read(buf, offset) {
  var res = 0, offset = offset || 0, shift = 0, counter = offset, b, l = buf.length;
  do {
    if (counter >= l) {
      read.bytes = 0;
      throw new RangeError('Could not decode varint');
    }
    b = buf[counter++];
    res += shift < 28 ? (b & REST$1) << shift : (b & REST$1) * Math.pow(2, shift);
    shift += 7;
  } while (b >= MSB$1);
  read.bytes = counter - offset;
  return res;
}
var N1 = Math.pow(2, 7);
var N2 = Math.pow(2, 14);
var N3 = Math.pow(2, 21);
var N4 = Math.pow(2, 28);
var N5 = Math.pow(2, 35);
var N6 = Math.pow(2, 42);
var N7 = Math.pow(2, 49);
var N8 = Math.pow(2, 56);
var N9 = Math.pow(2, 63);
var length = function (value) {
  return value < N1 ? 1 : value < N2 ? 2 : value < N3 ? 3 : value < N4 ? 4 : value < N5 ? 5 : value < N6 ? 6 : value < N7 ? 7 : value < N8 ? 8 : value < N9 ? 9 : 10;
};
var varint$1 = {
  encode: encode_1,
  decode: decode,
  encodingLength: length
};
var _brrp_varint = varint$1;
var varint$1$1 = _brrp_varint;

var varint_1 = varint$1$1;

var varint = createCommonjsModule(function (module, exports) {

Object.defineProperty(exports, '__esModule', { value: true });



const decode = data => {
  const code = varint_1.decode(data);
  return [
    code,
    varint_1.decode.bytes
  ];
};
const encodeTo = (int, target, offset = 0) => {
  varint_1.encode(int, target, offset);
  return target;
};
const encodingLength = int => {
  return varint_1.encodingLength(int);
};

exports.decode = decode;
exports.encodeTo = encodeTo;
exports.encodingLength = encodingLength;
});

var digest = createCommonjsModule(function (module, exports) {

Object.defineProperty(exports, '__esModule', { value: true });




const create = (code, digest) => {
  const size = digest.byteLength;
  const sizeOffset = varint.encodingLength(code);
  const digestOffset = sizeOffset + varint.encodingLength(size);
  const bytes = new Uint8Array(digestOffset + size);
  varint.encodeTo(code, bytes, 0);
  varint.encodeTo(size, bytes, sizeOffset);
  bytes.set(digest, digestOffset);
  return new Digest(code, size, digest, bytes);
};
const decode = multihash => {
  const bytes$1 = bytes.coerce(multihash);
  const [code, sizeOffset] = varint.decode(bytes$1);
  const [size, digestOffset] = varint.decode(bytes$1.subarray(sizeOffset));
  const digest = bytes$1.subarray(sizeOffset + digestOffset);
  if (digest.byteLength !== size) {
    throw new Error('Incorrect length');
  }
  return new Digest(code, size, digest, bytes$1);
};
const equals = (a, b) => {
  if (a === b) {
    return true;
  } else {
    return a.code === b.code && a.size === b.size && bytes.equals(a.bytes, b.bytes);
  }
};
class Digest {
  constructor(code, size, digest, bytes) {
    this.code = code;
    this.size = size;
    this.digest = digest;
    this.bytes = bytes;
  }
}

exports.Digest = Digest;
exports.create = create;
exports.decode = decode;
exports.equals = equals;
});

var hasher = createCommonjsModule(function (module, exports) {

Object.defineProperty(exports, '__esModule', { value: true });



const from = ({name, code, encode}) => new Hasher(name, code, encode);
class Hasher {
  constructor(name, code, encode) {
    this.name = name;
    this.code = code;
    this.encode = encode;
  }
  digest(input) {
    if (input instanceof Uint8Array) {
      const result = this.encode(input);
      return result instanceof Uint8Array ? digest.create(this.code, result) : result.then(digest$1 => digest.create(this.code, digest$1));
    } else {
      throw Error('Unknown type, must be binary type');
    }
  }
}

exports.Hasher = Hasher;
exports.from = from;
});

var sha2Browser = createCommonjsModule(function (module, exports) {

Object.defineProperty(exports, '__esModule', { value: true });



const sha = name => async data => new Uint8Array(await crypto.subtle.digest(name, data));
const sha256 = hasher.from({
  name: 'sha2-256',
  code: 18,
  encode: sha('SHA-256')
});
const sha512 = hasher.from({
  name: 'sha2-512',
  code: 19,
  encode: sha('SHA-512')
});

exports.sha256 = sha256;
exports.sha512 = sha512;
});

var identity_1 = createCommonjsModule(function (module, exports) {

Object.defineProperty(exports, '__esModule', { value: true });




const code = 0;
const name = 'identity';
const encode = bytes.coerce;
const digest$1 = input => digest.create(code, encode(input));
const identity = {
  code,
  name,
  encode,
  digest: digest$1
};

exports.identity = identity;
});

var raw = createCommonjsModule(function (module, exports) {

Object.defineProperty(exports, '__esModule', { value: true });



const name = 'raw';
const code = 85;
const encode = node => bytes.coerce(node);
const decode = data => bytes.coerce(data);

exports.code = code;
exports.decode = decode;
exports.encode = encode;
exports.name = name;
});

var json = createCommonjsModule(function (module, exports) {

Object.defineProperty(exports, '__esModule', { value: true });

const textEncoder = new TextEncoder();
const textDecoder = new TextDecoder();
const name = 'json';
const code = 512;
const encode = node => textEncoder.encode(JSON.stringify(node));
const decode = data => JSON.parse(textDecoder.decode(data));

exports.code = code;
exports.decode = decode;
exports.encode = encode;
exports.name = name;
});

var cid = createCommonjsModule(function (module, exports) {

Object.defineProperty(exports, '__esModule', { value: true });







class CID {
  constructor(version, code, multihash, bytes) {
    this.code = code;
    this.version = version;
    this.multihash = multihash;
    this.bytes = bytes;
    this.byteOffset = bytes.byteOffset;
    this.byteLength = bytes.byteLength;
    this.asCID = this;
    this._baseCache = new Map();
    Object.defineProperties(this, {
      byteOffset: hidden,
      byteLength: hidden,
      code: readonly,
      version: readonly,
      multihash: readonly,
      bytes: readonly,
      _baseCache: hidden,
      asCID: hidden
    });
  }
  toV0() {
    switch (this.version) {
    case 0: {
        return this;
      }
    default: {
        const {code, multihash} = this;
        if (code !== DAG_PB_CODE) {
          throw new Error('Cannot convert a non dag-pb CID to CIDv0');
        }
        if (multihash.code !== SHA_256_CODE) {
          throw new Error('Cannot convert non sha2-256 multihash CID to CIDv0');
        }
        return CID.createV0(multihash);
      }
    }
  }
  toV1() {
    switch (this.version) {
    case 0: {
        const {code, digest: digest$1} = this.multihash;
        const multihash = digest.create(code, digest$1);
        return CID.createV1(this.code, multihash);
      }
    case 1: {
        return this;
      }
    default: {
        throw Error(`Can not convert CID version ${ this.version } to version 0. This is a bug please report`);
      }
    }
  }
  equals(other) {
    return other && this.code === other.code && this.version === other.version && digest.equals(this.multihash, other.multihash);
  }
  toString(base) {
    const {bytes, version, _baseCache} = this;
    switch (version) {
    case 0:
      return toStringV0(bytes, _baseCache, base || base58.base58btc.encoder);
    default:
      return toStringV1(bytes, _baseCache, base || base32_1.base32.encoder);
    }
  }
  toJSON() {
    return {
      code: this.code,
      version: this.version,
      hash: this.multihash.bytes
    };
  }
  get [Symbol.toStringTag]() {
    return 'CID';
  }
  [Symbol.for('nodejs.util.inspect.custom')]() {
    return 'CID(' + this.toString() + ')';
  }
  static isCID(value) {
    deprecate(/^0\.0/, IS_CID_DEPRECATION);
    return !!(value && (value[cidSymbol] || value.asCID === value));
  }
  get toBaseEncodedString() {
    throw new Error('Deprecated, use .toString()');
  }
  get codec() {
    throw new Error('"codec" property is deprecated, use integer "code" property instead');
  }
  get buffer() {
    throw new Error('Deprecated .buffer property, use .bytes to get Uint8Array instead');
  }
  get multibaseName() {
    throw new Error('"multibaseName" property is deprecated');
  }
  get prefix() {
    throw new Error('"prefix" property is deprecated');
  }
  static asCID(value) {
    if (value instanceof CID) {
      return value;
    } else if (value != null && value.asCID === value) {
      const {version, code, multihash, bytes} = value;
      return new CID(version, code, multihash, bytes || encodeCID(version, code, multihash.bytes));
    } else if (value != null && value[cidSymbol] === true) {
      const {version, multihash, code} = value;
      const digest$1 = digest.decode(multihash);
      return CID.create(version, code, digest$1);
    } else {
      return null;
    }
  }
  static create(version, code, digest) {
    if (typeof code !== 'number') {
      throw new Error('String codecs are no longer supported');
    }
    switch (version) {
    case 0: {
        if (code !== DAG_PB_CODE) {
          throw new Error(`Version 0 CID must use dag-pb (code: ${ DAG_PB_CODE }) block encoding`);
        } else {
          return new CID(version, code, digest, digest.bytes);
        }
      }
    case 1: {
        const bytes = encodeCID(version, code, digest.bytes);
        return new CID(version, code, digest, bytes);
      }
    default: {
        throw new Error('Invalid version');
      }
    }
  }
  static createV0(digest) {
    return CID.create(0, DAG_PB_CODE, digest);
  }
  static createV1(code, digest) {
    return CID.create(1, code, digest);
  }
  static decode(bytes) {
    const [cid, remainder] = CID.decodeFirst(bytes);
    if (remainder.length) {
      throw new Error('Incorrect length');
    }
    return cid;
  }
  static decodeFirst(bytes$1) {
    const specs = CID.inspectBytes(bytes$1);
    const prefixSize = specs.size - specs.multihashSize;
    const multihashBytes = bytes.coerce(bytes$1.subarray(prefixSize, prefixSize + specs.multihashSize));
    if (multihashBytes.byteLength !== specs.multihashSize) {
      throw new Error('Incorrect length');
    }
    const digestBytes = multihashBytes.subarray(specs.multihashSize - specs.digestSize);
    const digest$1 = new digest.Digest(specs.multihashCode, specs.digestSize, digestBytes, multihashBytes);
    const cid = specs.version === 0 ? CID.createV0(digest$1) : CID.createV1(specs.codec, digest$1);
    return [
      cid,
      bytes$1.subarray(specs.size)
    ];
  }
  static inspectBytes(initialBytes) {
    let offset = 0;
    const next = () => {
      const [i, length] = varint.decode(initialBytes.subarray(offset));
      offset += length;
      return i;
    };
    let version = next();
    let codec = DAG_PB_CODE;
    if (version === 18) {
      version = 0;
      offset = 0;
    } else if (version === 1) {
      codec = next();
    }
    if (version !== 0 && version !== 1) {
      throw new RangeError(`Invalid CID version ${ version }`);
    }
    const prefixSize = offset;
    const multihashCode = next();
    const digestSize = next();
    const size = offset + digestSize;
    const multihashSize = size - prefixSize;
    return {
      version,
      codec,
      multihashCode,
      digestSize,
      multihashSize,
      size
    };
  }
  static parse(source, base) {
    const [prefix, bytes] = parseCIDtoBytes(source, base);
    const cid = CID.decode(bytes);
    cid._baseCache.set(prefix, source);
    return cid;
  }
}
const parseCIDtoBytes = (source, base) => {
  switch (source[0]) {
  case 'Q': {
      const decoder = base || base58.base58btc;
      return [
        base58.base58btc.prefix,
        decoder.decode(`${ base58.base58btc.prefix }${ source }`)
      ];
    }
  case base58.base58btc.prefix: {
      const decoder = base || base58.base58btc;
      return [
        base58.base58btc.prefix,
        decoder.decode(source)
      ];
    }
  case base32_1.base32.prefix: {
      const decoder = base || base32_1.base32;
      return [
        base32_1.base32.prefix,
        decoder.decode(source)
      ];
    }
  default: {
      if (base == null) {
        throw Error('To parse non base32 or base58btc encoded CID multibase decoder must be provided');
      }
      return [
        source[0],
        base.decode(source)
      ];
    }
  }
};
const toStringV0 = (bytes, cache, base) => {
  const {prefix} = base;
  if (prefix !== base58.base58btc.prefix) {
    throw Error(`Cannot string encode V0 in ${ base.name } encoding`);
  }
  const cid = cache.get(prefix);
  if (cid == null) {
    const cid = base.encode(bytes).slice(1);
    cache.set(prefix, cid);
    return cid;
  } else {
    return cid;
  }
};
const toStringV1 = (bytes, cache, base) => {
  const {prefix} = base;
  const cid = cache.get(prefix);
  if (cid == null) {
    const cid = base.encode(bytes);
    cache.set(prefix, cid);
    return cid;
  } else {
    return cid;
  }
};
const DAG_PB_CODE = 112;
const SHA_256_CODE = 18;
const encodeCID = (version, code, multihash) => {
  const codeOffset = varint.encodingLength(version);
  const hashOffset = codeOffset + varint.encodingLength(code);
  const bytes = new Uint8Array(hashOffset + multihash.byteLength);
  varint.encodeTo(version, bytes, 0);
  varint.encodeTo(code, bytes, codeOffset);
  bytes.set(multihash, hashOffset);
  return bytes;
};
const cidSymbol = Symbol.for('@ipld/js-cid/CID');
const readonly = {
  writable: false,
  configurable: false,
  enumerable: true
};
const hidden = {
  writable: false,
  enumerable: false,
  configurable: false
};
const version = '0.0.0-dev';
const deprecate = (range, message) => {
  if (range.test(version)) {
    console.warn(message);
  } else {
    throw new Error(message);
  }
};
const IS_CID_DEPRECATION = `CID.isCID(v) is deprecated and will be removed in the next major release.
Following code pattern:

if (CID.isCID(value)) {
  doSomethingWithCID(value)
}

Is replaced with:

const cid = CID.asCID(value)
if (cid) {
  // Make sure to use cid instead of value
  doSomethingWithCID(cid)
}
`;

exports.CID = CID;
});

createCommonjsModule(function (module, exports) {

Object.defineProperty(exports, '__esModule', { value: true });









exports.CID = cid.CID;
exports.varint = varint;
exports.bytes = bytes;
exports.hasher = hasher;
exports.digest = digest;
});

var basics = createCommonjsModule(function (module, exports) {

Object.defineProperty(exports, '__esModule', { value: true });





















const bases = {
  ...identity_1$1,
  ...base2_1,
  ...base8_1,
  ...base10_1,
  ...base16_1,
  ...base32_1,
  ...base36_1,
  ...base58,
  ...base64_1
};
const hashes = {
  ...sha2Browser,
  ...identity_1
};
const codecs = {
  raw,
  json
};

exports.CID = cid.CID;
exports.hasher = hasher;
exports.digest = digest;
exports.varint = varint;
exports.bytes = bytes;
exports.bases = bases;
exports.codecs = codecs;
exports.hashes = hashes;
});

function createCodec(name, prefix, encode, decode) {
  return {
    name,
    prefix,
    encoder: {
      name,
      prefix,
      encode
    },
    decoder: { decode }
  };
}
const string = createCodec('utf8', 'u', buf => {
  const decoder = new TextDecoder('utf8');
  return 'u' + decoder.decode(buf);
}, str => {
  const encoder = new TextEncoder();
  return encoder.encode(str.substring(1));
});
const ascii = createCodec('ascii', 'a', buf => {
  let string = 'a';
  for (let i = 0; i < buf.length; i++) {
    string += String.fromCharCode(buf[i]);
  }
  return string;
}, str => {
  str = str.substring(1);
  const buf = new Uint8Array(str.length);
  for (let i = 0; i < str.length; i++) {
    buf[i] = str.charCodeAt(i);
  }
  return buf;
});
const BASES = {
  utf8: string,
  'utf-8': string,
  hex: basics.bases.base16,
  latin1: ascii,
  ascii: ascii,
  binary: ascii,
  ...basics.bases
};

var bases = BASES;

var toString_1 = createCommonjsModule(function (module, exports) {

Object.defineProperty(exports, '__esModule', { value: true });



function toString(array, encoding = 'utf8') {
  const base = bases[encoding];
  if (!base) {
    throw new Error(`Unsupported encoding "${ encoding }"`);
  }
  return base.encoder.encode(array).substring(1);
}

exports.toString = toString;
});

var concat_1 = createCommonjsModule(function (module, exports) {

Object.defineProperty(exports, '__esModule', { value: true });

function concat(arrays, length) {
  if (!length) {
    length = arrays.reduce((acc, curr) => acc + curr.length, 0);
  }
  const output = new Uint8Array(length);
  let offset = 0;
  for (const arr of arrays) {
    output.set(arr, offset);
    offset += arr.length;
  }
  return output;
}

exports.concat = concat;
});

var __classPrivateFieldSet = (undefined && undefined.__classPrivateFieldSet) || function (receiver, privateMap, value) {
    if (!privateMap.has(receiver)) {
        throw new TypeError("attempted to set private field on non-instance");
    }
    privateMap.set(receiver, value);
    return value;
};
var __classPrivateFieldGet = (undefined && undefined.__classPrivateFieldGet) || function (receiver, privateMap) {
    if (!privateMap.has(receiver)) {
        throw new TypeError("attempted to get private field on non-instance");
    }
    return privateMap.get(receiver);
};
var __asyncValues = (undefined && undefined.__asyncValues) || function (o) {
    if (!Symbol.asyncIterator) throw new TypeError("Symbol.asyncIterator is not defined.");
    var m = o[Symbol.asyncIterator], i;
    return m ? m.call(o) : (o = typeof __values === "function" ? __values(o) : o[Symbol.iterator](), i = {}, verb("next"), verb("throw"), verb("return"), i[Symbol.asyncIterator] = function () { return this; }, i);
    function verb(n) { i[n] = o[n] && function (v) { return new Promise(function (resolve, reject) { v = o[n](v), settle(resolve, reject, v.done, v.value); }); }; }
    function settle(resolve, reject, d, v) { Promise.resolve(v).then(function(v) { resolve({ value: v, done: d }); }, reject); }
};
var _IPFS;
class Adapter {
    constructor(context) {
        _IPFS.set(this, void 0);
        __classPrivateFieldSet(this, _IPFS, context.IPFS);
        this.putAdapter = new IpfsPutAdapter(context);
    }
    async get(address) {
        var e_1, _a;
        const cid = address.toString();
        const chunks = [];
        try {
            // @ts-ignore
            for (var _b = __asyncValues(__classPrivateFieldGet(this, _IPFS).cat(cid)), _c; _c = await _b.next(), !_c.done;) {
                const chunk = _c.value;
                chunks.push(chunk);
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (_c && !_c.done && (_a = _b.return)) await _a.call(_b);
            }
            finally { if (e_1) throw e_1.error; }
        }
        const fileString = toString_1.toString(concat_1.concat(chunks));
        const fileJson = JSON.parse(fileString);
        //pin file to help persistence
        await __classPrivateFieldGet(this, _IPFS).pin.add(cid);
        return fileJson;
    }
}
_IPFS = new WeakMap();

var Icon = "'use strict';\n\nfunction noop() { }\nfunction run(fn) {\n    return fn();\n}\nfunction blank_object() {\n    return Object.create(null);\n}\nfunction run_all(fns) {\n    fns.forEach(run);\n}\nfunction is_function(thing) {\n    return typeof thing === 'function';\n}\nfunction safe_not_equal(a, b) {\n    return a != a ? b == b : a !== b || ((a && typeof a === 'object') || typeof a === 'function');\n}\nfunction is_empty(obj) {\n    return Object.keys(obj).length === 0;\n}\nfunction insert(target, node, anchor) {\n    target.insertBefore(node, anchor || null);\n}\nfunction detach(node) {\n    node.parentNode.removeChild(node);\n}\nfunction element(name) {\n    return document.createElement(name);\n}\nfunction listen(node, event, handler, options) {\n    node.addEventListener(event, handler, options);\n    return () => node.removeEventListener(event, handler, options);\n}\nfunction attr(node, attribute, value) {\n    if (value == null)\n        node.removeAttribute(attribute);\n    else if (node.getAttribute(attribute) !== value)\n        node.setAttribute(attribute, value);\n}\nfunction children(element) {\n    return Array.from(element.childNodes);\n}\nfunction set_input_value(input, value) {\n    input.value = value == null ? '' : value;\n}\nfunction attribute_to_object(attributes) {\n    const result = {};\n    for (const attribute of attributes) {\n        result[attribute.name] = attribute.value;\n    }\n    return result;\n}\n\nlet current_component;\nfunction set_current_component(component) {\n    current_component = component;\n}\n\nconst dirty_components = [];\nconst binding_callbacks = [];\nconst render_callbacks = [];\nconst flush_callbacks = [];\nconst resolved_promise = Promise.resolve();\nlet update_scheduled = false;\nfunction schedule_update() {\n    if (!update_scheduled) {\n        update_scheduled = true;\n        resolved_promise.then(flush);\n    }\n}\nfunction add_render_callback(fn) {\n    render_callbacks.push(fn);\n}\n// flush() calls callbacks in this order:\n// 1. All beforeUpdate callbacks, in order: parents before children\n// 2. All bind:this callbacks, in reverse order: children before parents.\n// 3. All afterUpdate callbacks, in order: parents before children. EXCEPT\n//    for afterUpdates called during the initial onMount, which are called in\n//    reverse order: children before parents.\n// Since callbacks might update component values, which could trigger another\n// call to flush(), the following steps guard against this:\n// 1. During beforeUpdate, any updated components will be added to the\n//    dirty_components array and will cause a reentrant call to flush(). Because\n//    the flush index is kept outside the function, the reentrant call will pick\n//    up where the earlier call left off and go through all dirty components. The\n//    current_component value is saved and restored so that the reentrant call will\n//    not interfere with the \"parent\" flush() call.\n// 2. bind:this callbacks cannot trigger new flush() calls.\n// 3. During afterUpdate, any updated components will NOT have their afterUpdate\n//    callback called a second time; the seen_callbacks set, outside the flush()\n//    function, guarantees this behavior.\nconst seen_callbacks = new Set();\nlet flushidx = 0; // Do *not* move this inside the flush() function\nfunction flush() {\n    const saved_component = current_component;\n    do {\n        // first, call beforeUpdate functions\n        // and update components\n        while (flushidx < dirty_components.length) {\n            const component = dirty_components[flushidx];\n            flushidx++;\n            set_current_component(component);\n            update(component.$$);\n        }\n        set_current_component(null);\n        dirty_components.length = 0;\n        flushidx = 0;\n        while (binding_callbacks.length)\n            binding_callbacks.pop()();\n        // then, once components are updated, call\n        // afterUpdate functions. This may cause\n        // subsequent updates...\n        for (let i = 0; i < render_callbacks.length; i += 1) {\n            const callback = render_callbacks[i];\n            if (!seen_callbacks.has(callback)) {\n                // ...so guard against infinite loops\n                seen_callbacks.add(callback);\n                callback();\n            }\n        }\n        render_callbacks.length = 0;\n    } while (dirty_components.length);\n    while (flush_callbacks.length) {\n        flush_callbacks.pop()();\n    }\n    update_scheduled = false;\n    seen_callbacks.clear();\n    set_current_component(saved_component);\n}\nfunction update($$) {\n    if ($$.fragment !== null) {\n        $$.update();\n        run_all($$.before_update);\n        const dirty = $$.dirty;\n        $$.dirty = [-1];\n        $$.fragment && $$.fragment.p($$.ctx, dirty);\n        $$.after_update.forEach(add_render_callback);\n    }\n}\nconst outroing = new Set();\nfunction transition_in(block, local) {\n    if (block && block.i) {\n        outroing.delete(block);\n        block.i(local);\n    }\n}\nfunction mount_component(component, target, anchor, customElement) {\n    const { fragment, on_mount, on_destroy, after_update } = component.$$;\n    fragment && fragment.m(target, anchor);\n    if (!customElement) {\n        // onMount happens before the initial afterUpdate\n        add_render_callback(() => {\n            const new_on_destroy = on_mount.map(run).filter(is_function);\n            if (on_destroy) {\n                on_destroy.push(...new_on_destroy);\n            }\n            else {\n                // Edge case - component was destroyed immediately,\n                // most likely as a result of a binding initialising\n                run_all(new_on_destroy);\n            }\n            component.$$.on_mount = [];\n        });\n    }\n    after_update.forEach(add_render_callback);\n}\nfunction destroy_component(component, detaching) {\n    const $$ = component.$$;\n    if ($$.fragment !== null) {\n        run_all($$.on_destroy);\n        $$.fragment && $$.fragment.d(detaching);\n        // TODO null out other refs, including component.$$ (but need to\n        // preserve final state?)\n        $$.on_destroy = $$.fragment = null;\n        $$.ctx = [];\n    }\n}\nfunction make_dirty(component, i) {\n    if (component.$$.dirty[0] === -1) {\n        dirty_components.push(component);\n        schedule_update();\n        component.$$.dirty.fill(0);\n    }\n    component.$$.dirty[(i / 31) | 0] |= (1 << (i % 31));\n}\nfunction init(component, options, instance, create_fragment, not_equal, props, append_styles, dirty = [-1]) {\n    const parent_component = current_component;\n    set_current_component(component);\n    const $$ = component.$$ = {\n        fragment: null,\n        ctx: null,\n        // state\n        props,\n        update: noop,\n        not_equal,\n        bound: blank_object(),\n        // lifecycle\n        on_mount: [],\n        on_destroy: [],\n        on_disconnect: [],\n        before_update: [],\n        after_update: [],\n        context: new Map(options.context || (parent_component ? parent_component.$$.context : [])),\n        // everything else\n        callbacks: blank_object(),\n        dirty,\n        skip_bound: false,\n        root: options.target || parent_component.$$.root\n    };\n    append_styles && append_styles($$.root);\n    let ready = false;\n    $$.ctx = instance\n        ? instance(component, options.props || {}, (i, ret, ...rest) => {\n            const value = rest.length ? rest[0] : ret;\n            if ($$.ctx && not_equal($$.ctx[i], $$.ctx[i] = value)) {\n                if (!$$.skip_bound && $$.bound[i])\n                    $$.bound[i](value);\n                if (ready)\n                    make_dirty(component, i);\n            }\n            return ret;\n        })\n        : [];\n    $$.update();\n    ready = true;\n    run_all($$.before_update);\n    // `false` as a special case of no DOM component\n    $$.fragment = create_fragment ? create_fragment($$.ctx) : false;\n    if (options.target) {\n        if (options.hydrate) {\n            const nodes = children(options.target);\n            // eslint-disable-next-line @typescript-eslint/no-non-null-assertion\n            $$.fragment && $$.fragment.l(nodes);\n            nodes.forEach(detach);\n        }\n        else {\n            // eslint-disable-next-line @typescript-eslint/no-non-null-assertion\n            $$.fragment && $$.fragment.c();\n        }\n        if (options.intro)\n            transition_in(component.$$.fragment);\n        mount_component(component, options.target, options.anchor, options.customElement);\n        flush();\n    }\n    set_current_component(parent_component);\n}\nlet SvelteElement;\nif (typeof HTMLElement === 'function') {\n    SvelteElement = class extends HTMLElement {\n        constructor() {\n            super();\n            this.attachShadow({ mode: 'open' });\n        }\n        connectedCallback() {\n            const { on_mount } = this.$$;\n            this.$$.on_disconnect = on_mount.map(run).filter(is_function);\n            // @ts-ignore todo: improve typings\n            for (const key in this.$$.slotted) {\n                // @ts-ignore todo: improve typings\n                this.appendChild(this.$$.slotted[key]);\n            }\n        }\n        attributeChangedCallback(attr, _oldValue, newValue) {\n            this[attr] = newValue;\n        }\n        disconnectedCallback() {\n            run_all(this.$$.on_disconnect);\n        }\n        $destroy() {\n            destroy_component(this, 1);\n            this.$destroy = noop;\n        }\n        $on(type, callback) {\n            // TODO should this delegate to addEventListener?\n            const callbacks = (this.$$.callbacks[type] || (this.$$.callbacks[type] = []));\n            callbacks.push(callback);\n            return () => {\n                const index = callbacks.indexOf(callback);\n                if (index !== -1)\n                    callbacks.splice(index, 1);\n            };\n        }\n        $set($$props) {\n            if (this.$$set && !is_empty($$props)) {\n                this.$$.skip_bound = true;\n                this.$$set($$props);\n                this.$$.skip_bound = false;\n            }\n        }\n    };\n}\n\n/* Icon.svelte generated by Svelte v3.46.3 */\n\nfunction create_if_block(ctx) {\n\tlet input;\n\tlet mounted;\n\tlet dispose;\n\n\treturn {\n\t\tc() {\n\t\t\tinput = element(\"input\");\n\t\t\tinput.disabled = true;\n\t\t},\n\t\tm(target, anchor) {\n\t\t\tinsert(target, input, anchor);\n\t\t\tset_input_value(input, /*string*/ ctx[1]);\n\n\t\t\tif (!mounted) {\n\t\t\t\tdispose = listen(input, \"input\", /*input_input_handler*/ ctx[2]);\n\t\t\t\tmounted = true;\n\t\t\t}\n\t\t},\n\t\tp(ctx, dirty) {\n\t\t\tif (dirty & /*string*/ 2 && input.value !== /*string*/ ctx[1]) {\n\t\t\t\tset_input_value(input, /*string*/ ctx[1]);\n\t\t\t}\n\t\t},\n\t\td(detaching) {\n\t\t\tif (detaching) detach(input);\n\t\t\tmounted = false;\n\t\t\tdispose();\n\t\t}\n\t};\n}\n\nfunction create_fragment(ctx) {\n\tlet div;\n\tlet if_block = /*expression*/ ctx[0] && create_if_block(ctx);\n\n\treturn {\n\t\tc() {\n\t\t\tdiv = element(\"div\");\n\t\t\tif (if_block) if_block.c();\n\t\t\tthis.c = noop;\n\t\t\tattr(div, \"class\", \"container\");\n\t\t},\n\t\tm(target, anchor) {\n\t\t\tinsert(target, div, anchor);\n\t\t\tif (if_block) if_block.m(div, null);\n\t\t},\n\t\tp(ctx, [dirty]) {\n\t\t\tif (/*expression*/ ctx[0]) {\n\t\t\t\tif (if_block) {\n\t\t\t\t\tif_block.p(ctx, dirty);\n\t\t\t\t} else {\n\t\t\t\t\tif_block = create_if_block(ctx);\n\t\t\t\t\tif_block.c();\n\t\t\t\t\tif_block.m(div, null);\n\t\t\t\t}\n\t\t\t} else if (if_block) {\n\t\t\t\tif_block.d(1);\n\t\t\t\tif_block = null;\n\t\t\t}\n\t\t},\n\t\ti: noop,\n\t\to: noop,\n\t\td(detaching) {\n\t\t\tif (detaching) detach(div);\n\t\t\tif (if_block) if_block.d();\n\t\t}\n\t};\n}\n\nfunction instance($$self, $$props, $$invalidate) {\n\tlet { expression } = $$props;\n\tlet string;\n\n\tfunction input_input_handler() {\n\t\tstring = this.value;\n\t\t($$invalidate(1, string), $$invalidate(0, expression));\n\t}\n\n\t$$self.$$set = $$props => {\n\t\tif ('expression' in $$props) $$invalidate(0, expression = $$props.expression);\n\t};\n\n\t$$self.$$.update = () => {\n\t\tif ($$self.$$.dirty & /*expression*/ 1) {\n\t\t\tif (expression && expression.data) $$invalidate(1, string = JSON.parse(expression.data));\n\t\t}\n\t};\n\n\treturn [expression, string, input_input_handler];\n}\n\nclass Icon extends SvelteElement {\n\tconstructor(options) {\n\t\tsuper();\n\t\tthis.shadowRoot.innerHTML = `<style>.container{color:burlywood;width:400px;height:300px}input{width:100%;height:300px;color:black;background:yellowgreen;text-align:center;font-size:20px}</style>`;\n\n\t\tinit(\n\t\t\tthis,\n\t\t\t{\n\t\t\t\ttarget: this.shadowRoot,\n\t\t\t\tprops: attribute_to_object(this.attributes),\n\t\t\t\tcustomElement: true\n\t\t\t},\n\t\t\tinstance,\n\t\t\tcreate_fragment,\n\t\t\tsafe_not_equal,\n\t\t\t{ expression: 0 },\n\t\t\tnull\n\t\t);\n\n\t\tif (options) {\n\t\t\tif (options.target) {\n\t\t\t\tinsert(options.target, this, options.anchor);\n\t\t\t}\n\n\t\t\tif (options.props) {\n\t\t\t\tthis.$set(options.props);\n\t\t\t\tflush();\n\t\t\t}\n\t\t}\n\t}\n\n\tstatic get observedAttributes() {\n\t\treturn [\"expression\"];\n\t}\n\n\tget expression() {\n\t\treturn this.$$.ctx[0];\n\t}\n\n\tset expression(expression) {\n\t\tthis.$$set({ expression });\n\t\tflush();\n\t}\n}\n\nmodule.exports = Icon;\n//# sourceMappingURL=Icon.js.map\n";

var ConstructorIcon = "'use strict';\n\nfunction noop() { }\nfunction run(fn) {\n    return fn();\n}\nfunction blank_object() {\n    return Object.create(null);\n}\nfunction run_all(fns) {\n    fns.forEach(run);\n}\nfunction is_function(thing) {\n    return typeof thing === 'function';\n}\nfunction safe_not_equal(a, b) {\n    return a != a ? b == b : a !== b || ((a && typeof a === 'object') || typeof a === 'function');\n}\nfunction is_empty(obj) {\n    return Object.keys(obj).length === 0;\n}\nfunction append(target, node) {\n    target.appendChild(node);\n}\nfunction insert(target, node, anchor) {\n    target.insertBefore(node, anchor || null);\n}\nfunction detach(node) {\n    node.parentNode.removeChild(node);\n}\nfunction element(name) {\n    return document.createElement(name);\n}\nfunction text(data) {\n    return document.createTextNode(data);\n}\nfunction space() {\n    return text(' ');\n}\nfunction listen(node, event, handler, options) {\n    node.addEventListener(event, handler, options);\n    return () => node.removeEventListener(event, handler, options);\n}\nfunction attr(node, attribute, value) {\n    if (value == null)\n        node.removeAttribute(attribute);\n    else if (node.getAttribute(attribute) !== value)\n        node.setAttribute(attribute, value);\n}\nfunction children(element) {\n    return Array.from(element.childNodes);\n}\nfunction set_input_value(input, value) {\n    input.value = value == null ? '' : value;\n}\nfunction attribute_to_object(attributes) {\n    const result = {};\n    for (const attribute of attributes) {\n        result[attribute.name] = attribute.value;\n    }\n    return result;\n}\n\nlet current_component;\nfunction set_current_component(component) {\n    current_component = component;\n}\n\nconst dirty_components = [];\nconst binding_callbacks = [];\nconst render_callbacks = [];\nconst flush_callbacks = [];\nconst resolved_promise = Promise.resolve();\nlet update_scheduled = false;\nfunction schedule_update() {\n    if (!update_scheduled) {\n        update_scheduled = true;\n        resolved_promise.then(flush);\n    }\n}\nfunction add_render_callback(fn) {\n    render_callbacks.push(fn);\n}\n// flush() calls callbacks in this order:\n// 1. All beforeUpdate callbacks, in order: parents before children\n// 2. All bind:this callbacks, in reverse order: children before parents.\n// 3. All afterUpdate callbacks, in order: parents before children. EXCEPT\n//    for afterUpdates called during the initial onMount, which are called in\n//    reverse order: children before parents.\n// Since callbacks might update component values, which could trigger another\n// call to flush(), the following steps guard against this:\n// 1. During beforeUpdate, any updated components will be added to the\n//    dirty_components array and will cause a reentrant call to flush(). Because\n//    the flush index is kept outside the function, the reentrant call will pick\n//    up where the earlier call left off and go through all dirty components. The\n//    current_component value is saved and restored so that the reentrant call will\n//    not interfere with the \"parent\" flush() call.\n// 2. bind:this callbacks cannot trigger new flush() calls.\n// 3. During afterUpdate, any updated components will NOT have their afterUpdate\n//    callback called a second time; the seen_callbacks set, outside the flush()\n//    function, guarantees this behavior.\nconst seen_callbacks = new Set();\nlet flushidx = 0; // Do *not* move this inside the flush() function\nfunction flush() {\n    const saved_component = current_component;\n    do {\n        // first, call beforeUpdate functions\n        // and update components\n        while (flushidx < dirty_components.length) {\n            const component = dirty_components[flushidx];\n            flushidx++;\n            set_current_component(component);\n            update(component.$$);\n        }\n        set_current_component(null);\n        dirty_components.length = 0;\n        flushidx = 0;\n        while (binding_callbacks.length)\n            binding_callbacks.pop()();\n        // then, once components are updated, call\n        // afterUpdate functions. This may cause\n        // subsequent updates...\n        for (let i = 0; i < render_callbacks.length; i += 1) {\n            const callback = render_callbacks[i];\n            if (!seen_callbacks.has(callback)) {\n                // ...so guard against infinite loops\n                seen_callbacks.add(callback);\n                callback();\n            }\n        }\n        render_callbacks.length = 0;\n    } while (dirty_components.length);\n    while (flush_callbacks.length) {\n        flush_callbacks.pop()();\n    }\n    update_scheduled = false;\n    seen_callbacks.clear();\n    set_current_component(saved_component);\n}\nfunction update($$) {\n    if ($$.fragment !== null) {\n        $$.update();\n        run_all($$.before_update);\n        const dirty = $$.dirty;\n        $$.dirty = [-1];\n        $$.fragment && $$.fragment.p($$.ctx, dirty);\n        $$.after_update.forEach(add_render_callback);\n    }\n}\nconst outroing = new Set();\nfunction transition_in(block, local) {\n    if (block && block.i) {\n        outroing.delete(block);\n        block.i(local);\n    }\n}\nfunction mount_component(component, target, anchor, customElement) {\n    const { fragment, on_mount, on_destroy, after_update } = component.$$;\n    fragment && fragment.m(target, anchor);\n    if (!customElement) {\n        // onMount happens before the initial afterUpdate\n        add_render_callback(() => {\n            const new_on_destroy = on_mount.map(run).filter(is_function);\n            if (on_destroy) {\n                on_destroy.push(...new_on_destroy);\n            }\n            else {\n                // Edge case - component was destroyed immediately,\n                // most likely as a result of a binding initialising\n                run_all(new_on_destroy);\n            }\n            component.$$.on_mount = [];\n        });\n    }\n    after_update.forEach(add_render_callback);\n}\nfunction destroy_component(component, detaching) {\n    const $$ = component.$$;\n    if ($$.fragment !== null) {\n        run_all($$.on_destroy);\n        $$.fragment && $$.fragment.d(detaching);\n        // TODO null out other refs, including component.$$ (but need to\n        // preserve final state?)\n        $$.on_destroy = $$.fragment = null;\n        $$.ctx = [];\n    }\n}\nfunction make_dirty(component, i) {\n    if (component.$$.dirty[0] === -1) {\n        dirty_components.push(component);\n        schedule_update();\n        component.$$.dirty.fill(0);\n    }\n    component.$$.dirty[(i / 31) | 0] |= (1 << (i % 31));\n}\nfunction init(component, options, instance, create_fragment, not_equal, props, append_styles, dirty = [-1]) {\n    const parent_component = current_component;\n    set_current_component(component);\n    const $$ = component.$$ = {\n        fragment: null,\n        ctx: null,\n        // state\n        props,\n        update: noop,\n        not_equal,\n        bound: blank_object(),\n        // lifecycle\n        on_mount: [],\n        on_destroy: [],\n        on_disconnect: [],\n        before_update: [],\n        after_update: [],\n        context: new Map(options.context || (parent_component ? parent_component.$$.context : [])),\n        // everything else\n        callbacks: blank_object(),\n        dirty,\n        skip_bound: false,\n        root: options.target || parent_component.$$.root\n    };\n    append_styles && append_styles($$.root);\n    let ready = false;\n    $$.ctx = instance\n        ? instance(component, options.props || {}, (i, ret, ...rest) => {\n            const value = rest.length ? rest[0] : ret;\n            if ($$.ctx && not_equal($$.ctx[i], $$.ctx[i] = value)) {\n                if (!$$.skip_bound && $$.bound[i])\n                    $$.bound[i](value);\n                if (ready)\n                    make_dirty(component, i);\n            }\n            return ret;\n        })\n        : [];\n    $$.update();\n    ready = true;\n    run_all($$.before_update);\n    // `false` as a special case of no DOM component\n    $$.fragment = create_fragment ? create_fragment($$.ctx) : false;\n    if (options.target) {\n        if (options.hydrate) {\n            const nodes = children(options.target);\n            // eslint-disable-next-line @typescript-eslint/no-non-null-assertion\n            $$.fragment && $$.fragment.l(nodes);\n            nodes.forEach(detach);\n        }\n        else {\n            // eslint-disable-next-line @typescript-eslint/no-non-null-assertion\n            $$.fragment && $$.fragment.c();\n        }\n        if (options.intro)\n            transition_in(component.$$.fragment);\n        mount_component(component, options.target, options.anchor, options.customElement);\n        flush();\n    }\n    set_current_component(parent_component);\n}\nlet SvelteElement;\nif (typeof HTMLElement === 'function') {\n    SvelteElement = class extends HTMLElement {\n        constructor() {\n            super();\n            this.attachShadow({ mode: 'open' });\n        }\n        connectedCallback() {\n            const { on_mount } = this.$$;\n            this.$$.on_disconnect = on_mount.map(run).filter(is_function);\n            // @ts-ignore todo: improve typings\n            for (const key in this.$$.slotted) {\n                // @ts-ignore todo: improve typings\n                this.appendChild(this.$$.slotted[key]);\n            }\n        }\n        attributeChangedCallback(attr, _oldValue, newValue) {\n            this[attr] = newValue;\n        }\n        disconnectedCallback() {\n            run_all(this.$$.on_disconnect);\n        }\n        $destroy() {\n            destroy_component(this, 1);\n            this.$destroy = noop;\n        }\n        $on(type, callback) {\n            // TODO should this delegate to addEventListener?\n            const callbacks = (this.$$.callbacks[type] || (this.$$.callbacks[type] = []));\n            callbacks.push(callback);\n            return () => {\n                const index = callbacks.indexOf(callback);\n                if (index !== -1)\n                    callbacks.splice(index, 1);\n            };\n        }\n        $set($$props) {\n            if (this.$$set && !is_empty($$props)) {\n                this.$$.skip_bound = true;\n                this.$$set($$props);\n                this.$$.skip_bound = false;\n            }\n        }\n    };\n}\n\n/* ConstructorIcon.svelte generated by Svelte v3.46.3 */\n\nfunction create_fragment(ctx) {\n\tlet div;\n\tlet input;\n\tlet t0;\n\tlet button0;\n\tlet t2;\n\tlet button1;\n\tlet mounted;\n\tlet dispose;\n\n\treturn {\n\t\tc() {\n\t\t\tdiv = element(\"div\");\n\t\t\tinput = element(\"input\");\n\t\t\tt0 = space();\n\t\t\tbutton0 = element(\"button\");\n\t\t\tbutton0.textContent = \"Commit\";\n\t\t\tt2 = space();\n\t\t\tbutton1 = element(\"button\");\n\t\t\tbutton1.textContent = \"Discard\";\n\t\t\tthis.c = noop;\n\t\t\tattr(div, \"class\", \"container\");\n\t\t},\n\t\tm(target, anchor) {\n\t\t\tinsert(target, div, anchor);\n\t\t\tappend(div, input);\n\t\t\tset_input_value(input, /*text*/ ctx[2]);\n\t\t\tappend(div, t0);\n\t\t\tappend(div, button0);\n\t\t\tappend(div, t2);\n\t\t\tappend(div, button1);\n\n\t\t\tif (!mounted) {\n\t\t\t\tdispose = [\n\t\t\t\t\tlisten(input, \"input\", /*input_input_handler*/ ctx[3]),\n\t\t\t\t\tlisten(button0, \"click\", /*click_handler*/ ctx[4]),\n\t\t\t\t\tlisten(button1, \"click\", function () {\n\t\t\t\t\t\tif (is_function(/*discard*/ ctx[1])) /*discard*/ ctx[1].apply(this, arguments);\n\t\t\t\t\t})\n\t\t\t\t];\n\n\t\t\t\tmounted = true;\n\t\t\t}\n\t\t},\n\t\tp(new_ctx, [dirty]) {\n\t\t\tctx = new_ctx;\n\n\t\t\tif (dirty & /*text*/ 4 && input.value !== /*text*/ ctx[2]) {\n\t\t\t\tset_input_value(input, /*text*/ ctx[2]);\n\t\t\t}\n\t\t},\n\t\ti: noop,\n\t\to: noop,\n\t\td(detaching) {\n\t\t\tif (detaching) detach(div);\n\t\t\tmounted = false;\n\t\t\trun_all(dispose);\n\t\t}\n\t};\n}\n\nfunction instance($$self, $$props, $$invalidate) {\n\tlet { commitExpression } = $$props;\n\tlet { discard } = $$props;\n\tlet text = \"\";\n\n\tfunction input_input_handler() {\n\t\ttext = this.value;\n\t\t$$invalidate(2, text);\n\t}\n\n\tconst click_handler = () => commitExpression(text);\n\n\t$$self.$$set = $$props => {\n\t\tif ('commitExpression' in $$props) $$invalidate(0, commitExpression = $$props.commitExpression);\n\t\tif ('discard' in $$props) $$invalidate(1, discard = $$props.discard);\n\t};\n\n\treturn [commitExpression, discard, text, input_input_handler, click_handler];\n}\n\nclass ConstructorIcon extends SvelteElement {\n\tconstructor(options) {\n\t\tsuper();\n\t\tthis.shadowRoot.innerHTML = `<style>.container{color:burlywood;width:400px;height:300px}input{width:100%;height:200px}</style>`;\n\n\t\tinit(\n\t\t\tthis,\n\t\t\t{\n\t\t\t\ttarget: this.shadowRoot,\n\t\t\t\tprops: attribute_to_object(this.attributes),\n\t\t\t\tcustomElement: true\n\t\t\t},\n\t\t\tinstance,\n\t\t\tcreate_fragment,\n\t\t\tsafe_not_equal,\n\t\t\t{ commitExpression: 0, discard: 1 },\n\t\t\tnull\n\t\t);\n\n\t\tif (options) {\n\t\t\tif (options.target) {\n\t\t\t\tinsert(options.target, this, options.anchor);\n\t\t\t}\n\n\t\t\tif (options.props) {\n\t\t\t\tthis.$set(options.props);\n\t\t\t\tflush();\n\t\t\t}\n\t\t}\n\t}\n\n\tstatic get observedAttributes() {\n\t\treturn [\"commitExpression\", \"discard\"];\n\t}\n\n\tget commitExpression() {\n\t\treturn this.$$.ctx[0];\n\t}\n\n\tset commitExpression(commitExpression) {\n\t\tthis.$$set({ commitExpression });\n\t\tflush();\n\t}\n\n\tget discard() {\n\t\treturn this.$$.ctx[1];\n\t}\n\n\tset discard(discard) {\n\t\tthis.$$set({ discard });\n\t\tflush();\n\t}\n}\n\nmodule.exports = ConstructorIcon;\n//# sourceMappingURL=ConstructorIcon.js.map\n";

class NoteExpressionUI {
    icon() {
        return Icon;
    }
    constructorIcon() {
        return ConstructorIcon;
    }
}

function interactions(expression) {
    return [];
}
function isImmutableExpression(expression) {
    return true;
}
function create(context) {
    const expressionAdapter = new Adapter(context);
    const expressionUI = new NoteExpressionUI();
    return {
        name: 'note-ipfs',
        expressionAdapter,
        expressionUI,
        interactions,
        isImmutableExpression
    };
}
const name = "note-ipfs";

exports["default"] = create;
exports.name = name;
//# sourceMappingURL=bundle.js.map
