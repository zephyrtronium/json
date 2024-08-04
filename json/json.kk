/*
   Copyright 2024 Branden J Brown

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

module json/json

import std/num/decimal
import std/text/parse

pub type json
  JNull
  JFalse
  JTrue
  JNum{x: decimal}
  JString{x: string}
  JArray{x: list<json>}
  JObject{x: list<(string, json)>}

fun ws(): parse ()
  ignore(chars-are("space", fn(c) c == ' ' || c == '\n' || c == '\r' || c == '\t'))

val ch = parse/char

fun pdecimal(): parse decimal
  // Copied from koka/lib/std/num/decimal, where it is unexported.
  val neg = sign()
  val whole = digits()
  val frac = optional("", { char('.'); digits() })
  val exp = optional(0, { one-of("eE"); pint() })
  val i = decimal((whole ++ frac).parse-int.default(0), exp - frac.count)
  if neg then ~i else i

fun ptoken(tok: string, r: json): parse json
  pstring(tok)
  r

fun pnull(): parse json
  ptoken("null", JNull)

fun pfalse(): parse json
  ptoken("false", JFalse)

fun ptrue(): parse json
  ptoken("true", JTrue)

fun pnum(): parse json
  JNum(pdecimal())

fun puni(): parse char
  fun pxdig() { char-is("string", is-hex-digit) }
  val (a, b, c, d) = (pxdig(), pxdig(), pxdig(), pxdig())
  fun xdig(u)
    if '0' <= u && u <= '9' then int(u - '0')
    elif 'A' <= u && u <= 'F' then int(u - 'A') + 10
    else int(u - 'a') + 10
  char(xdig(a)*0x1000 + xdig(b)*0x0100 + xdig(c)*0x0010 + xdig(d)*0x0001)

fun pjchar(): parse char
  val c = char-is("string", fn(c) c != '"' && c >= char(0x20))
  if c != '\\' then return c
  val e = one-of("\"\\/bfnrtu")
  match e
    'b' -> char(8)
    'f' -> char(12)
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    'u' -> puni()
    _ -> e

fun pstr(): parse json
  ch('"')
  val s = many(pjchar)
  ch('"')
  JString(s.string)

fun parry(): <parse,div> json
  ch('[')
  ws()
  if optional(char(0), { ch(']') }) == ']' then return JArray([])
  val e = pjson()
  ch(']')
  JNull

fun pobject(): <parse,div> json
  fun kv()
    val k = pstr()
    ws()
    ch(':')
    ws()
    val v = pjson()
    (k, v)
  ch('{')
  ws()
  if optional(char(0), { ch('}') }) == '}' then return JObject([])
  val e = kv()
  ch('}')
  JNull

pub fun pjson(): <parse,div> json
  val r = choose([
    pnull,
    pfalse,
    ptrue,
    pnum,
    pstr,
    parry,
    pobject,
  ])
  ws()
  r
