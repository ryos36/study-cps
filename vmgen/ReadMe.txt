:offset
(:global-varibale からのオフセット計算用)
  -> sym1 sym2 の offset を計算する
　-> その場で計算できるならすぐに計算する
  -> いまは 256 以上になるとたぶんバグる。
  -> imm8 で offset 1 は 4 byte 分の意味と解釈する

:label
  -> オフセット計算用

いずれも 
            (mark-label-offset vmgen value)
でlabel (あるいはoffset と)場所(オフセット。4の倍数ではない)の組を
assoc として記録。
例
:ADDRESS の例
((EXIT 37) (MAIN 5) (:GLOBAL-VARIABLE-POINTER 2))
あるいはこんな感じ
:LABEL の例
(((|:MAIN| 11 8) (|:EXIT| 12) (|:label0| 17) (|:label1| 25) (|:FIBO| 64)
                   (|:FIBO-2| 65)))

(normalize-codes0 (key x-pos-pair pos-func)
で修正するが、これらは本当の位置を知らなくてはならない。
それは label-pos-pair としてラベルがあるたびに記憶されている。
例えばこんな感じ

(:LABEL-POS-PAIR
 ((|:label0| . 61) (|:FIBO| . 49) (|:label1| . 38) (|:FIBO-2| . 21)
  (|:MAIN| . 15) (|:EXIT| . 13) (:GLOBAL-VARIABLE-POINTER . 13) (EXIT . 12)
  (MAIN . 11)))

これは elt の番号になっているので逐次置き換える。
例えば |:MAIN| は 11 と 8で使われている。
実際の |:MAIN| の offset は 15 である。
そこで、
(setf (elt pure-codes 11) 15)
(setf (elt pure-codes 8) 15)
で置き換わる。

(2016年  4月 27日 水曜日 00:30:04 JST)
**** 変換時に 4 の倍数にするように変更しようとおもったら。****
**** 変更先が 64bit 環境かそうでないかで下駄が違う。****
**** なので、抽象的な意味の inst の番号にするのが適当であると結論 ****
このため jump などの imm32 は inst 番号のままとなる。
つまり2bitのタグは無関係になる。

これは :LABEL と :ADDRESS に対して実行される。
この２つは imm32 しか存在しない。

:OFFSET は今のところ、置き換えをしていない。
(IMM8 を使っていてすぐに変換しているから)

----------------------------------------------------------------
operand(oprand?) のビット配置 8bit 毎に
flag r0 r1 r2 の順。

flag のビット配置(31ビット目から 24ビット)
--001122 (<= 数字はオペランド番号) (4/27に逆順にした)
の順 2bit で
00 なら :REG
01 なら :IMM8
10 なら :IMM32
11 は未定義
たとえば
--000100 なら オペランド 0 はレジスタ、1は IMM8、2 はレジスタ
--101000 なら オペランド 0 はレジスタ、1は IMM32、2 はIMM32

オペランド0 は通常 :REG
IMM32 が複数あると複雑なので record_set! 以外には適用していない。
だから record_set! 以外は オペランド2 も通常 :REG
--000000 か --000100 か --001000 になる。

:IMM8 のときは r2 がその値になる。
その場合、op によってい意味が異なることに注意。
例えば + ならそれは単純に数字。record_set! なら 4バイト単位のオフセット。
imm32 では数字もアドレスなどは下位 2bit がタグになる。
00 がアドレス系 01 が数字
movei32 は単純に reg にその値を入れる。中身の意味は問わない。

ただし、上記の flag は hvm 側で有効に使われていない。

----------------------------------------------------------------
形式を忘れないように書いておく

// little endian
struct vm_header {
    uint32_t magic_id;
    uint32_t header_size;

    uint32_t codes_offset;
    uint32_t codes_size;

    uint32_t label_offset_pos_offset;
    uint32_t label_offset_pos_size;

    uint32_t address_pos_offset;
    uint32_t address_pos_size;

    uint32_t insn_pos_offset;
    uint32_t insn_pos_size;

    uint32_t insn_declare_offset;
    uint32_t insn_declare_size;
};

codes_offset は vm_header 先頭からの実際のコードのオフセット(単位はバイト)。
codes_size はそのバイト数。

address_pos_offset
address_pos_size
単位はバイト。
アドレスを再配置するための 32bit ごと(4byte)にオフセットが書かれている。
オフセットの単位は 32bit 。なので、そこに書かれているオフセット値
は修正されなければならいが、
inst[*ui32p] というコードが内に inst からのオフセットが書かれている。
したがって、

inst[*ui32p] => instの先頭からのオフセット値を表しているので
inst[*ui32p].target = &inst[inst[*ui32p].i];
とするとちゃんとアドレスになる。

insn_declare_offset;
insn_declare_size;
からは名前情報が入っている。
4byte の name_size = 名前の個数

続いて
       struct np {
            uint16_t no;
            uint16_t size;

            char name[0];
        } __attribute__((packed)) *np = (struct np *)ui32p;

こんな感じで no と size と名前
no は先頭から 0 で始まる（重複情報）。
4バイトバウンダリで上記 np が続く。name は 0 ターミネートを保証している。
size は 0 を含むバイト数(4の倍数ではないことに注意)

例
name[0]:loadi32
name[1]:jump
name[2]:halt
name[3]:movei8
name[4]:record_set
name[5]:add
name[6]:addi8
name[7]:addi32
name[8]:record_refi8_address
name[9]:move

uint32_t insn_pos_offset;
uint32_t insn_pos_size;
実際の insn は上記 loadi32 などがどこに入っているかを情報として持っている。
それが　insn_pos_offset からの insn_pos_size 分の 32bit ずつの情報
例えば loadi32 は 4 9 15 など。 この場合 4, 9, 15 の insn を実際の
スレッデドコードのアドレスに置き換える。

このようにシンボルにしたのはスレッデドコードに変換可能にするため。

//
uint32_t label_offset_pos_offset;
uint32_t label_offset_pos_size;
この情報は使われていない。

label_offset_pos_offset から始まる 32bit の情報は
insn の配列番号(オフセット)であり、そこにオフセットが書かれている。
例
label_offset:vm_code[8]  = 0x100
label_offset:vm_code[11] = 0x08c
label_offset:vm_code[12] = 0x000
オフセットの値なので書き換える必要がない。
uint32_t label_offset_pos_size;
この情報は使われていない
----------------------------------------------------------------
形式を忘れないように書いておく

// little endian
struct vm_header {
    uint32_t magic_id;
    uint32_t header_size;

    uint32_t codes_offset;
    uint32_t codes_size;

    uint32_t label_offset_pos_offset;
    uint32_t label_offset_pos_size;

    uint32_t address_pos_offset;
    uint32_t address_pos_size;

    uint32_t insn_pos_offset;
    uint32_t insn_pos_size;

    uint32_t insn_declare_offset;
    uint32_t insn_declare_size;
};

codes_offset は vm_header 先頭からの実際のコードのオフセット(単位はバイト)。
codes_size はそのバイト数。

address_pos_offset
address_pos_size
単位はバイト。
アドレスを再配置するための 32bit ごと(4byte)にオフセットが書かれている。
オフセットの単位は 32bit 。なので、そこに書かれているオフセット値
は修正されなければならいが、
inst[*ui32p] というコードが内に inst からのオフセットが書かれている。
したがって、

inst[*ui32p] => instの先頭からのオフセット値を表しているので
inst[*ui32p].target = &inst[inst[*ui32p].i];
とするとちゃんとアドレスになる。

insn_declare_offset;
insn_declare_size;
からは名前情報が入っている。
4byte の name_size = 名前の個数

続いて
       struct np {
            uint16_t no;
            uint16_t size;

            char name[0];
        } __attribute__((packed)) *np = (struct np *)ui32p;

こんな感じで no と size と名前
no は先頭から 0 で始まる（重複情報）。
4バイトバウンダリで上記 np が続く。name は 0 ターミネートを保証している。
size は 0 を含むバイト数(4の倍数ではないことに注意)

例
name[0]:loadi32
name[1]:jump
name[2]:halt
name[3]:movei8
name[4]:record_set
name[5]:add
name[6]:addi8
name[7]:addi32
name[8]:record_refi8_address
name[9]:move

uint32_t insn_pos_offset;
uint32_t insn_pos_size;
実際の insn は上記 loadi32 などがどこに入っているかを情報として持っている。
それが　insn_pos_offset からの insn_pos_size 分の 32bit ずつの情報
例えば loadi32 は 4 9 15 など。 この場合 4, 9, 15 の insn を実際の
スレッデドコードのアドレスに置き換える。

このようにシンボルにしたのはスレッデドコードに変換可能にするため。

//
uint32_t label_offset_pos_offset;
uint32_t label_offset_pos_size;
この情報は使われていない。

label_offset_pos_offset から始まる 32bit の情報は
insn の配列番号(オフセット)であり、そこにオフセットが書かれている。
例
label_offset:vm_code[8]  = 0x100
label_offset:vm_code[11] = 0x08c
label_offset:vm_code[12] = 0x000
オフセットの値なので書き換える必要がない。
uint32_t label_offset_pos_size;
この情報は使われていない
