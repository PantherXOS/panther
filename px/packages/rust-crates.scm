;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>
;;;
;;; guix import -i px/packages/rust-crates.scm crate -f /path/to/Cargo.lock PACKAGE

(define-module (px packages rust-crates)
  #:use-module (guix build-system cargo)
  #:export (lookup-cargo-inputs))

(define rust-abnf-core-0.6.0
  (crate-source "abnf-core" "0.6.0"
                "1shdbi2ffzvyf6v3wwlgwp5sa58m22pqk716b6gnm40v0wgjs67c"))

(define rust-addr2line-0.21.0
  (crate-source "addr2line" "0.21.0"
                "1jx0k3iwyqr8klqbzk6kjvr496yd94aspis10vwsj5wy7gib4c4a"))

(define rust-adler-1.0.2
  (crate-source "adler" "1.0.2"
                "1zim79cvzd5yrkzl3nyfx0avijwgk9fqv3yrscdy1cc79ih02qpj"))

(define rust-adler2-2.0.0
  (crate-source "adler2" "2.0.0"
                "09r6drylvgy8vv8k20lnbvwq8gp09h7smfn6h1rxsy15pgh629si"))

(define rust-adler2-2.0.1
  (crate-source "adler2" "2.0.1"
                "1ymy18s9hs7ya1pjc9864l30wk8p2qfqdi7mhhcc5nfakxbij09j"))

(define rust-aes-0.8.4
  (crate-source "aes" "0.8.4"
                "1853796anlwp4kqim0s6wm1srl4ib621nm0cl2h3c8klsjkgfsdi"))

(define rust-ahash-0.8.11
  (crate-source "ahash" "0.8.11"
                "04chdfkls5xmhp1d48gnjsmglbqibizs3bpbj6rsj604m10si7g8"))

(define rust-ahash-0.8.12
  (crate-source "ahash" "0.8.12"
                "0xbsp9rlm5ki017c0w6ay8kjwinwm8knjncci95mii30rmwz25as"))

(define rust-aho-corasick-1.1.2
  (crate-source "aho-corasick" "1.1.2"
                "1w510wnixvlgimkx1zjbvlxh6xps2vjgfqgwf5a6adlbjp5rv5mj"))

(define rust-aho-corasick-1.1.3
  (crate-source "aho-corasick" "1.1.3"
                "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f"))

(define rust-aliasable-0.1.3
  (crate-source "aliasable" "0.1.3"
                "1z8548zdjlm4ps1k0d7x68lfdyji02crwcc9rw3q3bb106f643r5"))

(define rust-aligned-vec-0.6.4
  (crate-source "aligned-vec" "0.6.4"
                "16vnf78hvfix5cwzd5xs5a2g6afmgb4h7n6yfsc36bv0r22072fw"))

(define rust-allocator-api2-0.2.21
  (crate-source "allocator-api2" "0.2.21"
                "08zrzs022xwndihvzdn78yqarv2b9696y67i6h78nla3ww87jgb8"))

(define rust-android-system-properties-0.1.5
  (crate-source "android_system_properties" "0.1.5"
                "04b3wrz12837j7mdczqd95b732gw5q7q66cv4yn4646lvccp57l1"))

(define rust-android-tzdata-0.1.1
  (crate-source "android-tzdata" "0.1.1"
                "1w7ynjxrfs97xg3qlcdns4kgfpwcdv824g611fq32cag4cdr96g9"))

(define rust-ansi-colours-1.2.3
  (crate-source "ansi_colours" "1.2.3"
                "1zimwh84gs1r0g0chy6x5lm9v0ksxxlzwy8nyj80f6cq08zc9vhl"))

(define rust-anstream-0.6.18
  (crate-source "anstream" "0.6.18"
                "16sjk4x3ns2c3ya1x28a44kh6p47c7vhk27251i015hik1lm7k4a"))

(define rust-anstream-0.6.20
  (crate-source "anstream" "0.6.20"
                "14k1iqdf3dx7hdjllmql0j9sjxkwr1lfdddi3adzff0r7mjn7r9s"))

(define rust-anstyle-1.0.10
  (crate-source "anstyle" "1.0.10"
                "1yai2vppmd7zlvlrp9grwll60knrmscalf8l2qpfz8b7y5lkpk2m"))

(define rust-anstyle-1.0.13
  (crate-source "anstyle" "1.0.13"
                "0y2ynjqajpny6q0amvfzzgw0gfw3l47z85km4gvx87vg02lcr4ji"))

(define rust-anstyle-parse-0.2.6
  (crate-source "anstyle-parse" "0.2.6"
                "1acqayy22fwzsrvr6n0lz6a4zvjjcvgr5sm941m7m0b2fr81cb9v"))

(define rust-anstyle-parse-0.2.7
  (crate-source "anstyle-parse" "0.2.7"
                "1hhmkkfr95d462b3zf6yl2vfzdqfy5726ya572wwg8ha9y148xjf"))

(define rust-anstyle-query-1.1.2
  (crate-source "anstyle-query" "1.1.2"
                "036nm3lkyk43xbps1yql3583fp4hg3b1600is7mcyxs1gzrpm53r"))

(define rust-anstyle-query-1.1.4
  (crate-source "anstyle-query" "1.1.4"
                "1qir6d6fl5a4y2gmmw9a5w93ckwx6xn51aryd83p26zn6ihiy8wy"))

(define rust-anstyle-wincon-3.0.10
  (crate-source "anstyle-wincon" "3.0.10"
                "0ajz9wsf46a2l3pds7v62xbhq2cffj7wrilamkx2z8r28m0k61iy"))

(define rust-anstyle-wincon-3.0.6
  (crate-source "anstyle-wincon" "3.0.6"
                "099ir0w3lbpsp1nxdzbf4anq98ww8ykyc9pd1g03xgkj1v7dn291"))

(define rust-anyhow-1.0.100
  (crate-source "anyhow" "1.0.100"
                "0qbfmw4hhv2ampza1csyvf1jqjs2dgrj29cv3h3sh623c6qvcgm2"))

(define rust-anyhow-1.0.75
  (crate-source "anyhow" "1.0.75"
                "1rmcjkim91c5mw7h9wn8nv0k6x118yz0xg0z1q18svgn42mqqrm4"))

(define rust-anyhow-1.0.79
  (crate-source "anyhow" "1.0.79"
                "1ji5irqiwr8yprgqj8zvnli7zd7fz9kzaiddq44jnrl2l289h3h8"))

(define rust-arbitrary-1.4.2
  (crate-source "arbitrary" "1.4.2"
                "1wcbi4x7i3lzcrkjda4810nqv03lpmvfhb0a85xrq1mbqjikdl63"))

(define rust-arg-enum-proc-macro-0.3.4
  (crate-source "arg_enum_proc_macro" "0.3.4"
                "1sjdfd5a8j6r99cf0bpqrd6b160x9vz97y5rysycsjda358jms8a"))

(define rust-argh-0.1.13
  (crate-source "argh" "0.1.13"
                "0h6jzj4aqswk9x6w3lbb8kdskyf93v73wlrfk4pvhdlabhr1izrl"))

(define rust-argh-derive-0.1.13
  (crate-source "argh_derive" "0.1.13"
                "00vqfqgxqq6dd9jgbg9qhn12hh06qzsj1incv3ajklsh7awb5dxd"))

(define rust-argh-shared-0.1.13
  (crate-source "argh_shared" "0.1.13"
                "1xplhinnv139x2w2wknvnms7css6c99l8dw7jb1wvv9dr0y18r54"))

(define rust-ariadne-0.2.0
  (crate-source "ariadne" "0.2.0"
                "00jprfhqydhdnwibjzbcv7g0aj5zbyy0fislhz88hx9hhynx0zrn"))

(define rust-arrayref-0.3.9
  (crate-source "arrayref" "0.3.9"
                "1jzyp0nvp10dmahaq9a2rnxqdd5wxgbvp8xaibps3zai8c9fi8kn"))

(define rust-arrayvec-0.7.6
  (crate-source "arrayvec" "0.7.6"
                "0l1fz4ccgv6pm609rif37sl5nv5k6lbzi7kkppgzqzh1vwix20kw"))

(define rust-async-broadcast-0.7.2
  (crate-source "async-broadcast" "0.7.2"
                "0ckmqcwyqwbl2cijk1y4r0vy60i89gqc86ijrxzz5f2m4yjqfnj3"))

(define rust-async-channel-2.3.1
  (crate-source "async-channel" "2.3.1"
                "0skvwxj6ysfc6d7bhczz9a2550260g62bm5gl0nmjxxyn007id49"))

(define rust-async-executor-1.13.1
  (crate-source "async-executor" "1.13.1"
                "1v6w1dbvsmw6cs4dk4lxj5dvrikc6xi479wikwaab2qy3h09mjih"))

(define rust-async-fs-2.1.2
  (crate-source "async-fs" "2.1.2"
                "0jp0p7lg9zqy2djgdmivbzx0yqmfn9sm2s9dkhaws3zlharhkkgb"))

(define rust-async-io-2.4.0
  (crate-source "async-io" "2.4.0"
                "0n8h0vy53n4vdkq529scqnkzm9vcl3r73za9nj81s2nfrhiv78j3"))

(define rust-async-lock-3.4.0
  (crate-source "async-lock" "3.4.0"
                "060vh45i809wcqyxzs5g69nqiqah7ydz0hpkcjys9258vqn4fvpz"))

(define rust-async-process-2.3.0
  (crate-source "async-process" "2.3.0"
                "1fr6cpqdw7hrmzns1983lgx86cg8vyz7nlrn0h0125iqq8fmy9b3"))

(define rust-async-recursion-1.1.1
  (crate-source "async-recursion" "1.1.1"
                "04ac4zh8qz2xjc79lmfi4jlqj5f92xjvfaqvbzwkizyqd4pl4hrv"))

(define rust-async-signal-0.2.10
  (crate-source "async-signal" "0.2.10"
                "1wxrq3871l00mil43nmh0akvwjjjnv0bn7n2pzwbvh00k0s00zk3"))

(define rust-async-task-4.7.1
  (crate-source "async-task" "4.7.1"
                "1pp3avr4ri2nbh7s6y9ws0397nkx1zymmcr14sq761ljarh3axcb"))

(define rust-async-trait-0.1.73
  (crate-source "async-trait" "0.1.73"
                "1w60x18qm18drm8pdl0ix4jai83nh8hlwfjswca3dh4096rww05w"))

(define rust-async-trait-0.1.85
  (crate-source "async-trait" "0.1.85"
                "0mm0gwad44zs7mna4a0m1z4dhzpmydfj73w4wm23c8xpnhrli4rz"))

(define rust-atomic-waker-1.1.2
  (crate-source "atomic-waker" "1.1.2"
                "1h5av1lw56m0jf0fd3bchxq8a30xv0b4wv8s4zkp4s0i7mfvs18m"))

(define rust-autocfg-1.1.0
  (crate-source "autocfg" "1.1.0"
                "1ylp3cb47ylzabimazvbz9ms6ap784zhb6syaz6c1jqpmcmq0s6l"))

(define rust-autocfg-1.4.0
  (crate-source "autocfg" "1.4.0"
                "09lz3by90d2hphbq56znag9v87gfpd9gb8nr82hll8z6x2nhprdc"))

(define rust-autocfg-1.5.0
  (crate-source "autocfg" "1.5.0"
                "1s77f98id9l4af4alklmzq46f21c980v13z2r1pcxx6bqgw0d1n0"))

(define rust-av1-grain-0.2.4
  (crate-source "av1-grain" "0.2.4"
                "1j409cf560kvhxzjs65ksnzbxz7k6sm5lywizan11ijvm0ngngjg"))

(define rust-avif-serialize-0.8.6
  (crate-source "avif-serialize" "0.8.6"
                "0gx2yqlh0j5063vk70hv0f1yq40lm6kvc45qx2gm3x1iz30gpj27"))

(define rust-backtrace-0.3.69
  (crate-source "backtrace" "0.3.69"
                "0dsq23dhw4pfndkx2nsa1ml2g31idm7ss7ljxp8d57avygivg290"))

(define rust-backtrace-0.3.71
  (crate-source "backtrace" "0.3.71"
                "17bgd7pbjb9gc8q47qwsg2lmy9i62x3bsjmmnjrwh5z8s805ic16"))

(define rust-base16ct-0.2.0
  (crate-source "base16ct" "0.2.0"
                "1kylrjhdzk7qpknrvlphw8ywdnvvg39dizw9622w3wk5xba04zsc"))

(define rust-base64-0.13.1
  (crate-source "base64" "0.13.1"
                "1s494mqmzjb766fy1kqlccgfg2sdcjb6hzbvzqv2jw65fdi5h6wy"))

(define rust-base64-0.21.3
  (crate-source "base64" "0.21.3"
                "0lvf1ishhckkjwiamhqr3iwy5ddrzgvgqfkblwkcaxrxqvxwwka1"))

(define rust-base64-0.21.7
  (crate-source "base64" "0.21.7"
                "0rw52yvsk75kar9wgqfwgb414kvil1gn7mqkrhn9zf1537mpsacx"))

(define rust-base64-0.22.1
  (crate-source "base64" "0.22.1"
                "1imqzgh7bxcikp5vx3shqvw9j09g9ly0xr0jma0q66i52r7jbcvj"))

(define rust-base64ct-1.6.0
  (crate-source "base64ct" "1.6.0"
                "0nvdba4jb8aikv60az40x2w1y96sjdq8z3yp09rwzmkhiwv1lg4c"))

(define rust-bet-1.0.4
  (crate-source "bet" "1.0.4"
                "1dhay8ypq3gy7hbzkfn4633r5sn1q4cc1yfzzvngcz9pk1dcscsa"))

(define rust-bincode-1.3.3
  (crate-source "bincode" "1.3.3"
                "1bfw3mnwzx5g1465kiqllp5n4r10qrqy88kdlp3jfwnq2ya5xx5i"))

(define rust-bit-field-0.10.3
  (crate-source "bit_field" "0.10.3"
                "1ikhbph4ap4w692c33r8bbv6yd2qxm1q3f64845grp1s6b3l0jqy"))

(define rust-bit-set-0.5.3
  (crate-source "bit-set" "0.5.3"
                "1wcm9vxi00ma4rcxkl3pzzjli6ihrpn9cfdi0c5b4cvga2mxs007"))

(define rust-bit-vec-0.6.3
  (crate-source "bit-vec" "0.6.3"
                "1ywqjnv60cdh1slhz67psnp422md6jdliji6alq0gmly2xm9p7rl"))

(define rust-bitfield-0.14.0
  (crate-source "bitfield" "0.14.0"
                "1b26k9acwss4qvrl60lf9n83l17d4hj47n5rmpd3iigf9j9n0zid"))

(define rust-bitflags-1.3.2
  (crate-source "bitflags" "1.3.2"
                "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))

(define rust-bitflags-2.4.0
  (crate-source "bitflags" "2.4.0"
                "0dc6xa7flfl59makmhixjcrslwlvdxxwrgxbr8p7bkvz53k2ls5l"))

(define rust-bitflags-2.4.2
  (crate-source "bitflags" "2.4.2"
                "1pqd142hyqlzr7p9djxq2ff0jx07a2sb2xp9lhw69cbf80s0jmzd"))

(define rust-bitflags-2.7.0
  (crate-source "bitflags" "2.7.0"
                "1gp1bjw29d12ij9nx0v02nc78s9d04zmyrwzspn4blyncwmg9qqv"))

(define rust-bitflags-2.9.4
  (crate-source "bitflags" "2.9.4"
                "157kkcv8s7vk6d17dar1pa5cqcz4c8pdrn16wm1ld7jnr86d2q92"))

(define rust-bitstream-io-2.6.0
  (crate-source "bitstream-io" "2.6.0"
                "1cli390l1dhp9skygyjjnqvczp36b7f31mkx9ry3dg26330cv6b0"))

(define rust-block-0.1.6
  (crate-source "block" "0.1.6"
                "16k9jgll25pzsq14f244q22cdv0zb4bqacldg3kx6h89d7piz30d"))

(define rust-block-buffer-0.10.4
  (crate-source "block-buffer" "0.10.4"
                "0w9sa2ypmrsqqvc20nhwr75wbb5cjr4kkyhpjm1z1lv2kdicfy1h"))

(define rust-block-padding-0.3.3
  (crate-source "block-padding" "0.3.3"
                "14wdad0r1qk5gmszxqd8cky6vx8qg7c153jv981mixzrpzmlz2d8"))

(define rust-block2-0.5.1
  (crate-source "block2" "0.5.1"
                "0pyiha5his2grzqr3mynmq244laql2j20992i59asp0gy7mjw4rc"))

(define rust-blocking-1.6.1
  (crate-source "blocking" "1.6.1"
                "1si99l8zp7c4zq87y35ayjgc5c9b60jb8h0k14zfcs679z2l2gvh"))

(define rust-blowfish-0.9.1
  (crate-source "blowfish" "0.9.1"
                "1mw7bvj3bg5w8vh9xw9xawqh7ixk2xwsxkj34ph96b9b1z6y44p4"))

(define rust-bluer-0.16.1
  (crate-source "bluer" "0.16.1"
                "0wz3nqxfbkjzxzn6r8p4z7ids36fk254fpjb1x5gk6v6c29rdmxf"))

(define rust-bounded-static-0.8.0
  (crate-source "bounded-static" "0.8.0"
                "08ck16yyfpmhbzwhik8nif6n7bgr6gkfnpjxzg53pd29m8yr1sqb"))

(define rust-bounded-static-derive-0.8.0
  (crate-source "bounded-static-derive" "0.8.0"
                "1yhv26kijlv27br1j67rj2ilck0kgbj7zybm96hpmmg54w70bbz0"))

(define rust-bstr-1.11.3
  (crate-source "bstr" "1.11.3"
                "1q3g2wmrvclgx7lk2p6mpzhqxzx41hyg962gkmlyxql1liar26jk"))

(define rust-bstr-1.12.0
  (crate-source "bstr" "1.12.0"
                "195i0gd7r7jg7a8spkmw08492n7rmiabcvz880xn2z8dkp8i6h93"))

(define rust-buffer-redux-1.0.2
  (crate-source "buffer-redux" "1.0.2"
                "1waq39blrj7j6qp1sp2fvplwmq10yhks7fgbsdy8kxdrqn3wz2jf"))

(define rust-build-rs-0.1.2
  (crate-source "build-rs" "0.1.2"
                "1lkxq4q9l3gg5jdsfca58rjq5x8647c0v2hv23czi6cccriqf2xh"))

(define rust-built-0.7.7
  (crate-source "built" "0.7.7"
                "0ywn0m11xm80pg6zrzq3sdj3vmzg3qs6baqnvfmkd377ly8n3van"))

(define rust-bumpalo-3.16.0
  (crate-source "bumpalo" "3.16.0"
                "0b015qb4knwanbdlp1x48pkb4pm57b8gidbhhhxr900q2wb6fabr"))

(define rust-bumpalo-3.19.0
  (crate-source "bumpalo" "3.19.0"
                "0hsdndvcpqbjb85ghrhska2qxvp9i75q2vb70hma9fxqawdy9ia6"))

(define rust-bytemuck-1.23.2
  (crate-source "bytemuck" "1.23.2"
                "0xs637lsr9p73ackjkmbjw80dp1dfdw0ydhdk0gzjcnzpkpfm59r"))

(define rust-byteorder-1.5.0
  (crate-source "byteorder" "1.5.0"
                "0jzncxyf404mwqdbspihyzpkndfgda450l0893pz5xj685cg5l0z"))

(define rust-byteorder-lite-0.1.0
  (crate-source "byteorder-lite" "0.1.0"
                "15alafmz4b9az56z6x7glcbcb6a8bfgyd109qc3bvx07zx4fj7wg"))

(define rust-bytes-1.4.0
  (crate-source "bytes" "1.4.0"
                "1gkh3fk4fm9xv5znlib723h5md5sxsvbd5113sbxff6g1lmgvcl9"))

(define rust-bytes-1.5.0
  (crate-source "bytes" "1.5.0"
                "08w2i8ac912l8vlvkv3q51cd4gr09pwlg3sjsjffcizlrb0i5gd2"))

(define rust-bytes-1.9.0
  (crate-source "bytes" "1.9.0"
                "16ykzx24v1x4f42v2lxyvlczqhdfji3v7r4ghwckpwijzvb1hn9j"))

(define rust-cairo-rs-0.18.5
  (crate-source "cairo-rs" "0.18.5"
                "1qjfkcq3mrh3p01nnn71dy3kn99g21xx3j8xcdvzn8ll2pq6x8lc"))

(define rust-cairo-sys-rs-0.18.2
  (crate-source "cairo-sys-rs" "0.18.2"
                "0lfsxl7ylw3phbnwmz3k58j1gnqi6kc2hdc7g3bb7f4hwnl9yp38"))

(define rust-camellia-0.1.0
  (crate-source "camellia" "0.1.0"
                "0c6f61rf0gzq7x9d2qmp0330pb397aldwdpmwqybbwly9rby4r1j"))

(define rust-camino-1.1.6
  (crate-source "camino" "1.1.6"
                "171vzfyrm2jmajd70q1m774297y028kadgm7cfw4kxc8lfsr57n5"))

(define rust-cast5-0.11.1
  (crate-source "cast5" "0.11.1"
                "04crg8dj6lxbp3lmdc3filsahxcyvccvhm0gx40g1k5i7mkpvc16"))

(define rust-cbc-0.1.2
  (crate-source "cbc" "0.1.2"
                "19l9y9ccv1ffg6876hshd123f2f8v7zbkc4nkckqycxf8fajmd96"))

(define rust-cc-1.0.83
  (crate-source "cc" "1.0.83"
                "1l643zidlb5iy1dskc5ggqs4wqa29a02f44piczqc8zcnsq4y5zi"))

(define rust-cc-1.2.16
  (crate-source "cc" "1.2.16"
                "131bhgafc1i86vvjipkj0kwzz0hlpwrkl8mdbmzyq2g69calqwdy"))

(define rust-cc-1.2.39
  (crate-source "cc" "1.2.39"
                "0py3546wz3k5qi6pbfz80jvg0g3qgzr21c7a1p5wjvscjm4l6dg1"))

(define rust-cc-1.2.8
  (crate-source "cc" "1.2.8"
                "0nm5z96vzzq4gbivfbmwm10lcl4qlyxwczpfqrrwfi6y3zlzc35d"))

(define rust-cesu8-1.1.0
  (crate-source "cesu8" "1.1.0"
                "0g6q58wa7khxrxcxgnqyi9s1z2cjywwwd3hzr5c55wskhx6s0hvd"))

(define rust-cfb-mode-0.8.2
  (crate-source "cfb-mode" "0.8.2"
                "0c6kd34jk4p52vr0qgn9slj6zdgmc42gfcqr6mqhmy37g138v2vk"))

(define rust-cfg-aliases-0.2.1
  (crate-source "cfg_aliases" "0.2.1"
                "092pxdc1dbgjb6qvh83gk56rkic2n2ybm4yvy76cgynmzi3zwfk1"))

(define rust-cfg-expr-0.15.7
  (crate-source "cfg-expr" "0.15.7"
                "07cny7rg58mlzmzw2b1x5b6ib1za9647gklksnlzv9m9cj5qcl7s"))

(define rust-cfg-expr-0.15.8
  (crate-source "cfg-expr" "0.15.8"
                "00lgf717pmf5qd2qsxxzs815v6baqg38d6m5i6wlh235p14asryh"))

(define rust-cfg-if-1.0.0
  (crate-source "cfg-if" "1.0.0"
                "1za0vb97n4brpzpv8lsbnzmq5r8f2b0cpqqr0sy8h5bn751xxwds"))

(define rust-cfg-if-1.0.3
  (crate-source "cfg-if" "1.0.3"
                "1afg7146gbxjvkbjx7i5sdrpqp9q5akmk9004fr8rsm90jf2il9g"))

(define rust-char-reader-0.1.1
  (crate-source "char_reader" "0.1.1"
                "0fpdhx6061a92fj3kw11rwv2zkdlx8z59gbkq7baf762vqi9p99p"))

(define rust-chrono-0.4.39
  (crate-source "chrono" "0.4.39"
                "09g8nf409lb184kl9j4s85k0kn8wzgjkp5ls9zid50b886fwqdky"))

(define rust-chrono-0.4.42
  (crate-source "chrono" "0.4.42"
                "1lp8iz9js9jwxw0sj8yi59v54lgvwdvm49b9wch77f25sfym4l0l"))

(define rust-chumsky-1.0.0-alpha.7
  (crate-source "chumsky" "1.0.0-alpha.7"
                "07qgyayw2vyrgfzfp4b0qsiwvadvacnm99kcqnfpi1kgk1v05f67"))

(define rust-cipher-0.4.4
  (crate-source "cipher" "0.4.4"
                "1b9x9agg67xq5nq879z66ni4l08m6m3hqcshk37d4is4ysd3ngvp"))

(define rust-clap-4.5.26
  (crate-source "clap" "4.5.26"
                "10v7qvn90calfbhap1c4r249i5c7fbxj09fn3szfz9pkis85xsx8"))

(define rust-clap-4.5.48
  (crate-source "clap" "4.5.48"
                "1bjz3d7bavy13ph2a6rm3c9y02ak70b195xakii7h6q2xarln4z2"))

(define rust-clap-builder-4.5.26
  (crate-source "clap_builder" "4.5.26"
                "08f1mzcvi7zjhm7hvz6al4jnv70ccqhwiaq74hihlspwnl0iic4n"))

(define rust-clap-builder-4.5.48
  (crate-source "clap_builder" "4.5.48"
                "1jaxnr7ik25r4yxgz657vm8kz62f64qmwxhplmzxz9n0lfpn9fn2"))

(define rust-clap-complete-4.5.42
  (crate-source "clap_complete" "4.5.42"
                "1l7zqm45vmy2jjsk0h0izgl74wyl39jvbs30wrmlpyjhwxlf99rk"))

(define rust-clap-complete-4.5.58
  (crate-source "clap_complete" "4.5.58"
                "0jmg0idg96cvx51l35ypia1np3q7sfj5wqxvi7kjs59fmlr0pgvm"))

(define rust-clap-derive-4.5.24
  (crate-source "clap_derive" "4.5.24"
                "131ih3dm76srkbpfx7zfspp9b556zgzj31wqhl0ji2b39lcmbdsl"))

(define rust-clap-derive-4.5.47
  (crate-source "clap_derive" "4.5.47"
                "174z9g13s85la2nmi8gv8ssjwz77im3rqg5isiinw6hg1fp7xzdv"))

(define rust-clap-help-1.5.0
  (crate-source "clap-help" "1.5.0"
                "0nwqnry884f6vky61y8vjkynyya4yggnz7gh7p9rq86s7bknxn59"))

(define rust-clap-lex-0.7.4
  (crate-source "clap_lex" "0.7.4"
                "19nwfls5db269js5n822vkc8dw0wjq2h1wf0hgr06ld2g52d2spl"))

(define rust-clap-lex-0.7.5
  (crate-source "clap_lex" "0.7.5"
                "0xb6pjza43irrl99axbhs12pxq4sr8x7xd36p703j57f5i3n2kxr"))

(define rust-clap-mangen-0.2.26
  (crate-source "clap_mangen" "0.2.26"
                "0fn1svjqm3znajji679nc2kfwm9lkyl73lzknf4rnkqlkgx44j3j"))

(define rust-clap-mangen-0.2.29
  (crate-source "clap_mangen" "0.2.29"
                "1qkr87xz9mgvh9mqqqqni7fw8dffzihmzwj7rfnxkw1h9g2w7d17"))

(define rust-cli-log-2.1.0
  (crate-source "cli-log" "2.1.0"
                "0ip0lir1dc5g0706isx48zhqq4034kjygy2ll1rx8p1rwm3al872"))

(define rust-clipboard-macos-0.1.1
  (crate-source "clipboard_macos" "0.1.1"
                "13zkbmnsl2c98452kvia7vr2ywwl123bj2q81diw78vv0jm4lzwv"))

(define rust-clipboard-win-4.5.0
  (crate-source "clipboard-win" "4.5.0"
                "0qh3rypkf1lazniq4nr04hxsck0d55rigb5sjvpvgnap4dyc54bi"))

(define rust-color-eyre-0.6.3
  (crate-source "color-eyre" "0.6.3"
                "1m9shifr9sdw0drszzyhvaq5jysrsiki44bl7m1gfdzj8rg6y52m"))

(define rust-color-quant-1.1.0
  (crate-source "color_quant" "1.1.0"
                "12q1n427h2bbmmm1mnglr57jaz2dj9apk0plcxw7nwqiai7qjyrx"))

(define rust-color-spantrace-0.2.1
  (crate-source "color-spantrace" "0.2.1"
                "1hkjgaqixrishwiq3lxxy1d4c3mvlv6avcj3ifwy50p3lyrf2syd"))

(define rust-colorchoice-1.0.3
  (crate-source "colorchoice" "1.0.3"
                "1439m3r3jy3xqck8aa13q658visn71ki76qa93cy55wkmalwlqsv"))

(define rust-colorchoice-1.0.4
  (crate-source "colorchoice" "1.0.4"
                "0x8ymkz1xr77rcj1cfanhf416pc4v681gmkc9dzb3jqja7f62nxh"))

(define rust-colored-2.1.0
  (crate-source "colored" "2.1.0"
                "1f4h9p64snrnz4x432iza15p4diqjcgpmpvhi956d6r1rq61bwnb"))

(define rust-combine-4.6.7
  (crate-source "combine" "4.6.7"
                "1z8rh8wp59gf8k23ar010phgs0wgf5i8cx4fg01gwcnzfn5k0nms"))

(define rust-comfy-table-7.1.3
  (crate-source "comfy-table" "7.1.3"
                "1nd4ns4vimypk554vqjww3iq14mdjbaawn5q1jl6w9j3nvknbw94"))

(define rust-concurrent-queue-2.5.0
  (crate-source "concurrent-queue" "2.5.0"
                "0wrr3mzq2ijdkxwndhf79k952cp4zkz35ray8hvsxl96xrx1k82c"))

(define rust-const-oid-0.9.6
  (crate-source "const-oid" "0.9.6"
                "1y0jnqaq7p2wvspnx7qj76m7hjcqpz73qzvr9l2p9n2s51vr6if2"))

(define rust-conv-0.3.3
  (crate-source "conv" "0.3.3"
                "168j1npqrif1yqxbgbk0pdrx9shzhs5ylc5a4xw49b6hbxi11zvq"))

(define rust-convert-case-0.7.1
  (crate-source "convert_case" "0.7.1"
                "1rzih8qbd3xh87wp76nkjvnrimn7vlzcwl2n88898ml59j6jnh5v"))

(define rust-coolor-1.1.0
  (crate-source "coolor" "1.1.0"
                "1wr7q2c8l1cmigw3h7yfdpwcz5g5xbwkirsvbjhdchxgwkyjl34q"))

(define rust-core-foundation-0.10.0
  (crate-source "core-foundation" "0.10.0"
                "0qscay14s2rwkg8nd8ljhiaf149hj8sfy95d70zssy64r3jp2lmm"))

(define rust-core-foundation-0.9.3
  (crate-source "core-foundation" "0.9.3"
                "0ii1ihpjb30fk38gdikm5wqlkmyr8k46fh4k2r8sagz5dng7ljhr"))

(define rust-core-foundation-0.9.4
  (crate-source "core-foundation" "0.9.4"
                "13zvbbj07yk3b61b8fhwfzhy35535a583irf23vlcg59j7h9bqci"))

(define rust-core-foundation-sys-0.8.4
  ;; TODO: Check bundled sources.
  (crate-source "core-foundation-sys" "0.8.4"
                "1yhf471qj6snnm2mcswai47vsbc9w30y4abmdp4crb4av87sb5p4"))

(define rust-core-foundation-sys-0.8.7
  ;; TODO: Check bundled sources.
  (crate-source "core-foundation-sys" "0.8.7"
                "12w8j73lazxmr1z0h98hf3z623kl8ms7g07jch7n4p8f9nwlhdkp"))

(define rust-core-maths-0.1.1
  (crate-source "core_maths" "0.1.1"
                "0c0dv11ixxpc9bsx5xasvl98mb1dlprzcm6qq6ls3nsygw0mwx3p"))

(define rust-cpufeatures-0.2.16
  (crate-source "cpufeatures" "0.2.16"
                "1hy466fkhxjbb16i7na95wz8yr14d0kd578pwzj5lbkz14jh5f0n"))

(define rust-cpufeatures-0.2.9
  (crate-source "cpufeatures" "0.2.9"
                "1wg1vmsx3gd30xkc7h7r6nfx7njx063hqjimgyrb0qj17bzpcyx1"))

(define rust-crc24-0.1.6
  (crate-source "crc24" "0.1.6"
                "1876c92swdpq1iv8j3y21vvfar96pxayn8rhvl42rf1yrx0if4px"))

(define rust-crc32fast-1.4.2
  (crate-source "crc32fast" "1.4.2"
                "1czp7vif73b8xslr3c9yxysmh9ws2r8824qda7j47ffs9pcnjxx9"))

(define rust-crc32fast-1.5.0
  (crate-source "crc32fast" "1.5.0"
                "04d51liy8rbssra92p0qnwjw8i9rm9c4m3bwy19wjamz1k4w30cl"))

(define rust-crokey-1.3.0
  (crate-source "crokey" "1.3.0"
                "1basm3p007xmbak9p9gic0lad1yk8g7sx0kcqwhdzcxyxd9hhdji"))

(define rust-crokey-proc-macros-1.3.0
  (crate-source "crokey-proc_macros" "1.3.0"
                "0fg41ah18lpgkwilh2sbjvp89kx90pr9fsl2l3hfappbr8ksgw9v"))

(define rust-crossbeam-0.8.4
  (crate-source "crossbeam" "0.8.4"
                "1a5c7yacnk723x0hfycdbl91ks2nxhwbwy46b8y5vyy0gxzcsdqi"))

(define rust-crossbeam-channel-0.5.13
  (crate-source "crossbeam-channel" "0.5.13"
                "1wkx45r34v7g3wyi3lg2wz536lrrrab4h4hh741shfhr8rlhsj1k"))

(define rust-crossbeam-channel-0.5.15
  (crate-source "crossbeam-channel" "0.5.15"
                "1cicd9ins0fkpfgvz9vhz3m9rpkh6n8d3437c3wnfsdkd3wgif42"))

(define rust-crossbeam-deque-0.8.6
  (crate-source "crossbeam-deque" "0.8.6"
                "0l9f1saqp1gn5qy0rxvkmz4m6n7fc0b3dbm6q1r5pmgpnyvi3lcx"))

(define rust-crossbeam-epoch-0.9.18
  (crate-source "crossbeam-epoch" "0.9.18"
                "03j2np8llwf376m3fxqx859mgp9f83hj1w34153c7a9c7i5ar0jv"))

(define rust-crossbeam-queue-0.3.12
  (crate-source "crossbeam-queue" "0.3.12"
                "059igaxckccj6ndmg45d5yf7cm4ps46c18m21afq3pwiiz1bnn0g"))

(define rust-crossbeam-utils-0.8.20
  (crate-source "crossbeam-utils" "0.8.20"
                "100fksq5mm1n7zj242cclkw6yf7a4a8ix3lvpfkhxvdhbda9kv12"))

(define rust-crossbeam-utils-0.8.21
  (crate-source "crossbeam-utils" "0.8.21"
                "0a3aa2bmc8q35fb67432w16wvi54sfmb69rk9h5bhd18vw0c99fh"))

(define rust-crossterm-0.25.0
  (crate-source "crossterm" "0.25.0"
                "0rsbkkhdf61aipc06b7vpl4cw3wnxz0miizp0ms3a5rcpq7nqkp6"))

(define rust-crossterm-0.27.0
  (crate-source "crossterm" "0.27.0"
                "1pr413ki440xgddlmkrc4j1bfx1h8rpmll87zn8ykja1bm2gwxpl"))

(define rust-crossterm-0.28.1
  (crate-source "crossterm" "0.28.1"
                "1im9vs6fvkql0sr378dfr4wdm1rrkrvr22v4i8byz05k1dd9b7c2"))

(define rust-crossterm-0.29.0
  (crate-source "crossterm" "0.29.0"
                "0yzqxxd90k7d2ac26xq1awsznsaq0qika2nv1ik3p0vzqvjg5ffq"))

(define rust-crossterm-winapi-0.9.1
  (crate-source "crossterm_winapi" "0.9.1"
                "0axbfb2ykbwbpf1hmxwpawwfs8wvmkcka5m561l7yp36ldi7rpdc"))

(define rust-crunchy-0.2.4
  (crate-source "crunchy" "0.2.4"
                "1mbp5navim2qr3x48lyvadqblcxc1dm0lqr0swrkkwy2qblvw3s6"))

(define rust-crypto-bigint-0.5.5
  (crate-source "crypto-bigint" "0.5.5"
                "0xmbdff3g6ii5sbxjxc31xfkv9lrmyril4arh3dzckd4gjsjzj8d"))

(define rust-crypto-common-0.1.6
  (crate-source "crypto-common" "0.1.6"
                "1cvby95a6xg7kxdz5ln3rl9xh66nz66w46mm3g56ri1z5x815yqv"))

(define rust-cstr-argument-0.1.2
  (crate-source "cstr-argument" "0.1.2"
                "0h7bi8sfvwq11r40pjvs3bqqmwva25dw7rasjp73niwscn79rgdn"))

(define rust-csv-1.3.1
  (crate-source "csv" "1.3.1"
                "1bzxgbbhy27flcyafxbj7f1hbn7b8wac04ijfgj34ry9m61lip5c"))

(define rust-csv-core-0.1.12
  (crate-source "csv-core" "0.1.12"
                "0gfrjjlfagarhyclxrqv6b14iaxgvgc8kmwwdvw08racvaqg60kx"))

(define rust-csv2svg-0.2.3
  (crate-source "csv2svg" "0.2.3"
                "0wf1ybnnrp85gnygbgi5s5inzm9yyzrc284nivxwhw4mrzgadhhw"))

(define rust-curve25519-dalek-4.1.3
  (crate-source "curve25519-dalek" "4.1.3"
                "1gmjb9dsknrr8lypmhkyjd67p1arb8mbfamlwxm7vph38my8pywp"))

(define rust-curve25519-dalek-derive-0.1.1
  (crate-source "curve25519-dalek-derive" "0.1.1"
                "1cry71xxrr0mcy5my3fb502cwfxy6822k4pm19cwrilrg7hq4s7l"))

(define rust-custom-debug-0.5.1
  (crate-source "custom_debug" "0.5.1"
                "1qn81isz144p531j3q4pi9lw15k0rvim0p6hjnay4ax4qbi0m7p8"))

(define rust-custom-debug-derive-0.5.1
  (crate-source "custom_debug_derive" "0.5.1"
                "1xxsw5la1397b8kqfr4a1adxwjbph9s2jy52x8ngdj9l2aag7a88"))

(define rust-custom-derive-0.1.7
  (crate-source "custom_derive" "0.1.7"
                "1f81bavw1wnykwh21hh4yyzigs6zl6f6pkk9p3car8kq95yfb2pg"))

(define rust-custom-error-1.9.2
  (crate-source "custom_error" "1.9.2"
                "19mwa90z1hgwn3mqj07b4cy6j4yc8c59k2n99mdvm9kz37fm32jg"))

(define rust-darling-0.10.2
  (crate-source "darling" "0.10.2"
                "0n7qsp6854wm3y1q1lvylhv15zvc87ibbac1nyfmcdbyv1snww0d"))

(define rust-darling-0.14.4
  (crate-source "darling" "0.14.4"
                "0l1qrn805bsxa0iy7x8bmdwr8c10hlw0yiqs8ckv7lbz86rhqxbv"))

(define rust-darling-core-0.10.2
  (crate-source "darling_core" "0.10.2"
                "16sija1jv0l754x4aa6b6fy01d1kf8m0r4id3flqipm45np61jgh"))

(define rust-darling-core-0.14.4
  (crate-source "darling_core" "0.14.4"
                "1w4b2ndxmkwghwq84yphk8x15jnpivm08w596g12ry5pwsk1r70h"))

(define rust-darling-macro-0.10.2
  (crate-source "darling_macro" "0.10.2"
                "0wlv31cxkrjijz5gv13hvk55c9lmd781aj12c8n84sa9mksa5dfr"))

(define rust-darling-macro-0.14.4
  (crate-source "darling_macro" "0.14.4"
                "13mlyd5w275c815k0ijf6g4c446hs8b3m2h4an5isqgpr7dv9am4"))

(define rust-data-encoding-2.6.0
  (crate-source "data-encoding" "2.6.0"
                "1qnn68n4vragxaxlkqcb1r28d3hhj43wch67lm4rpxlw89wnjmp8"))

(define rust-data-url-0.3.2
  (crate-source "data-url" "0.3.2"
                "0xl30jidc8s3kh2z3nvnn1nyzhbq5b2wpiqwzj9gjdrndk50n7my"))

(define rust-dbus-0.9.7
  (crate-source "dbus" "0.9.7"
                "06vdv4aarjs4w6byg9nqajr67c8qvlhk3153ic2i65pvp63ikchv"))

(define rust-dbus-crossroads-0.5.2
  (crate-source "dbus-crossroads" "0.5.2"
                "1q3dyywazr3hppm052fa8q2366q66ml789r42jjlnm47f51q6k1s"))

(define rust-dbus-secret-service-4.0.3
  (crate-source "dbus-secret-service" "4.0.3"
                "0jyqdya8zgddgwsrzf2gs9qqwz9043qv2idffgnjmnc18hvicaml"))

(define rust-dbus-tokio-0.7.6
  (crate-source "dbus-tokio" "0.7.6"
                "04xd3z2dnjv4d45kj3wqnwbnwllrp1zsg8v3q0qp2rxwb7a8hxh0"))

(define rust-der-0.7.9
  (crate-source "der" "0.7.9"
                "1h4vzjfa1lczxdf8avfj9qlwh1qianqlxdy1g5rn762qnvkzhnzm"))

(define rust-deranged-0.3.8
  (crate-source "deranged" "0.3.8"
                "0ikrhil2621rz9haakphdzrx035qwr175f639p8qyrazjj56wsgj"))

(define rust-deranged-0.5.4
  (crate-source "deranged" "0.5.4"
                "0wch36gpg2crz2f72p7c0i5l4bzxjkwxw96sdj57c1cadzw566d4"))

(define rust-derive-builder-0.12.0
  (crate-source "derive_builder" "0.12.0"
                "1y4p569zcvpmly5s5hmjp9h83drxvdp6kj6bb61h225mhj3pfrwd"))

(define rust-derive-builder-core-0.12.0
  (crate-source "derive_builder_core" "0.12.0"
                "03vvmw3mfg370swq0dh2h5kcjjb8va2m4asqgp9wfyy4l08xq6y1"))

(define rust-derive-builder-macro-0.12.0
  (crate-source "derive_builder_macro" "0.12.0"
                "17p71qzh7x1q2yxzz3xrg73zw3xl0h479b7ybyjm0s1rg9fa7kgb"))

(define rust-derive-more-2.0.1
  (crate-source "derive_more" "2.0.1"
                "0y3n97cc7rsvgnj211p92y1ppzh6jzvq5kvk6340ghkhfp7l4ch9"))

(define rust-derive-more-impl-2.0.1
  (crate-source "derive_more-impl" "2.0.1"
                "1wqxcb7d5lzvpplz9szp4rwy1r23f5wmixz0zd2vcjscqknji9mx"))

(define rust-des-0.8.1
  (crate-source "des" "0.8.1"
                "07kshslxanmg0g6007scvglfhg6mli2a8qzhx4kxx4z9ik781pgz"))

(define rust-deser-hjson-2.2.4
  (crate-source "deser-hjson" "2.2.4"
                "1qc82vh683wb3359fs5r9fwi37wjnb17zfmrwkbxw22w172am53x"))

(define rust-digest-0.10.7
  (crate-source "digest" "0.10.7"
                "14p2n6ih29x81akj097lvz7wi9b6b9hvls0lwrv7b6xwyy0s5ncy"))

(define rust-directories-4.0.1
  (crate-source "directories" "4.0.1"
                "045jbj5y2f1fmjs9rfcw95y0vjydb2rqqhz1sdnqhdmxv96ms77m"))

(define rust-directories-5.0.1
  (crate-source "directories" "5.0.1"
                "0dba6xzk79s1clqzxh2qlgzk3lmvvks1lzzjhhi3hd70hhxifjcs"))

(define rust-dirs-4.0.0
  (crate-source "dirs" "4.0.0"
                "0n8020zl4f0frfnzvgb9agvk4a14i1kjz4daqnxkgslndwmaffna"))

(define rust-dirs-5.0.1
  (crate-source "dirs" "5.0.1"
                "0992xk5vx75b2x91nw9ssb51mpl8x73j9rxmpi96cryn0ffmmi24"))

(define rust-dirs-sys-0.3.7
  ;; TODO: Check bundled sources.
  (crate-source "dirs-sys" "0.3.7"
                "19md1cnkazham8a6kh22v12d8hh3raqahfk6yb043vrjr68is78v"))

(define rust-dirs-sys-0.4.1
  ;; TODO: Check bundled sources.
  (crate-source "dirs-sys" "0.4.1"
                "071jy0pvaad9lsa6mzawxrh7cmr7hsmsdxwzm7jzldfkrfjha3sj"))

(define rust-displaydoc-0.2.4
  (crate-source "displaydoc" "0.2.4"
                "0p8pyg10csc782qlwx3znr6qx46ni96m1qh597kmyrf6s3s8axa8"))

(define rust-displaydoc-0.2.5
  (crate-source "displaydoc" "0.2.5"
                "1q0alair462j21iiqwrr21iabkfnb13d6x5w95lkdg21q2xrqdlp"))

(define rust-doc-comment-0.3.3
  (crate-source "doc-comment" "0.3.3"
                "043sprsf3wl926zmck1bm7gw0jq50mb76lkpk49vasfr6ax1p97y"))

(define rust-document-features-0.2.11
  (crate-source "document-features" "0.2.11"
                "0pdhpbz687fk2rkgz45yy3gvbhlxliwb7g1lj3jbx1f1qr89n94m"))

(define rust-domain-0.10.3
  (crate-source "domain" "0.10.3"
                "13f2l5g9890v1ilgn6z16y09p1as7a7ssa6dcf5aidpkv5k8c034"))

(define rust-dyn-clone-1.0.17
  (crate-source "dyn-clone" "1.0.17"
                "09cig7dgg6jnqa10p4233nd8wllbjf4ffsw7wj0m4lwa5w3z0vhd"))

(define rust-ecdsa-0.16.9
  (crate-source "ecdsa" "0.16.9"
                "1jhb0bcbkaz4001sdmfyv8ajrv8a1cg7z7aa5myrd4jjbhmz69zf"))

(define rust-ed25519-2.2.3
  (crate-source "ed25519" "2.2.3"
                "0lydzdf26zbn82g7xfczcac9d7mzm3qgx934ijjrd5hjpjx32m8i"))

(define rust-ed25519-dalek-2.1.1
  (crate-source "ed25519-dalek" "2.1.1"
                "0w88cafwglg9hjizldbmlza0ns3hls81zk1bcih3m5m3h67algaa"))

(define rust-either-1.13.0
  (crate-source "either" "1.13.0"
                "1w2c1mybrd7vljyxk77y9f4w9dyjrmp3yp82mk7bcm8848fazcb0"))

(define rust-either-1.15.0
  (crate-source "either" "1.15.0"
                "069p1fknsmzn9llaizh77kip0pqmcwpdsykv2x30xpjyija5gis8"))

(define rust-elliptic-curve-0.13.8
  (crate-source "elliptic-curve" "0.13.8"
                "0ixx4brgnzi61z29r3g1606nh2za88hzyz8c5r3p6ydzhqq09rmm"))

(define rust-email-address-0.2.9
  (crate-source "email_address" "0.2.9"
                "0jf4v3npa524c7npy7w3jl0a6gng26f51a4bgzs3jqna12dz2yg0"))

(define rust-email-lib-0.26.4
  (crate-source "email-lib" "0.26.4"
                "00pydx36j9k7f1lrbc5scxqr15j6a9msc7ad76yf4a3hggaqs1j0"))

(define rust-email-macros-0.0.2
  (crate-source "email-macros" "0.0.2"
                "15bw0jpdcgfybq9b6ak2mfdwn5c76l92r8gqcj3py0jissgs090g"))

(define rust-encoding-rs-0.8.35
  (crate-source "encoding_rs" "0.8.35"
                "1wv64xdrr9v37rqqdjsyb8l8wzlcbab80ryxhrszvnj59wy0y0vm"))

(define rust-endi-1.1.0
  (crate-source "endi" "1.1.0"
                "1gxp388g2zzbncp3rdn60wxkr49xbhhx94nl9p4a6c41w4ma7n53"))

(define rust-enum-as-inner-0.6.1
  (crate-source "enum-as-inner" "0.6.1"
                "1g3cywc65d9w974l2xy86ij13njss3qjc7b0kfbzbws9qrjs5rm1"))

(define rust-enumflags2-0.7.10
  (crate-source "enumflags2" "0.7.10"
                "0g8kmhaqxq44v76wh971biljrgaqbjc8fbyw2d1z3wsnb5zxncnj"))

(define rust-enumflags2-derive-0.7.10
  (crate-source "enumflags2_derive" "0.7.10"
                "1s29iqx3gj5l5s19a22lpn0nljhp5l9smqac99126n2qhfhlh3fy"))

(define rust-equator-0.4.2
  (crate-source "equator" "0.4.2"
                "1z760z5r0haxjyakbqxvswrz9mq7c29arrivgq8y1zldhc9v44a7"))

(define rust-equator-macro-0.4.2
  (crate-source "equator-macro" "0.4.2"
                "1cqzx3cqn9rxln3a607xr54wippzff56zs5chqdf3z2bnks3rwj4"))

(define rust-equivalent-1.0.1
  (crate-source "equivalent" "1.0.1"
                "1malmx5f4lkfvqasz319lq6gb3ddg19yzf9s8cykfsgzdmyq0hsl"))

(define rust-equivalent-1.0.2
  (crate-source "equivalent" "1.0.2"
                "03swzqznragy8n0x31lqc78g2af054jwivp7lkrbrc0khz74lyl7"))

(define rust-erased-serde-0.4.5
  (crate-source "erased-serde" "0.4.5"
                "13dirfj9972nvk05b20w3xyn3xp1j6qyfp9avhksnkxbcnfkiqi4"))

(define rust-errno-0.3.10
  (crate-source "errno" "0.3.10"
                "0pgblicz1kjz9wa9m0sghkhh2zw1fhq1mxzj7ndjm746kg5m5n1k"))

(define rust-errno-0.3.14
  (crate-source "errno" "0.3.14"
                "1szgccmh8vgryqyadg8xd58mnwwicf39zmin3bsn63df2wbbgjir"))

(define rust-errno-0.3.3
  (crate-source "errno" "0.3.3"
                "1pfv4gygg742cwi21gw88h4f7q5kvwkpk7b3xxpmrqh8hlc2cr8k"))

(define rust-errno-dragonfly-0.1.2
  (crate-source "errno-dragonfly" "0.1.2"
                "1grrmcm6q8512hkq5yzch3yv8wafflc2apbmsaabiyk44yqz2s5a"))

(define rust-error-code-2.3.1
  (crate-source "error-code" "2.3.1"
                "08baxlf8qz01lgjsdbfhs193r9y1nlc566s5xvzyf4dzwy8qkwb4"))

(define rust-euclid-0.22.11
  (crate-source "euclid" "0.22.11"
                "0j4yb01x9dn5hbbbigd3mwdplv4m29k5drmhmc95lj3yfi5xp75d"))

(define rust-event-listener-5.3.1
  (crate-source "event-listener" "5.3.1"
                "1fkm6q4hjn61wl52xyqyyxai0x9w0ngrzi0wf1qsf8vhsadvwck0"))

(define rust-event-listener-5.4.0
  (crate-source "event-listener" "5.4.0"
                "1bii2gn3vaa33s0gr2zph7cagiq0ppcfxcxabs24ri9z9kgar4il"))

(define rust-event-listener-strategy-0.5.2
  (crate-source "event-listener-strategy" "0.5.2"
                "18f5ri227khkayhv3ndv7yl4rnasgwksl2jhwgafcxzr7324s88g"))

(define rust-event-listener-strategy-0.5.3
  (crate-source "event-listener-strategy" "0.5.3"
                "1ch5gf6knllyq12jkb5zdfag573dh44307q4pwwi2g37sc6lwgiw"))

(define rust-exr-1.73.0
  (crate-source "exr" "1.73.0"
                "1q47yq78q9k210r6jy1wwrilxwwxqavik9l3l426rd17k7srfcgq"))

(define rust-eyre-0.6.12
  (crate-source "eyre" "0.6.12"
                "1v1a3vb9gs5zkwp4jzkcfnpg0gvyp4ifydzx37f4qy14kzcibnbw"))

(define rust-fallible-iterator-0.3.0
  (crate-source "fallible-iterator" "0.3.0"
                "0ja6l56yka5vn4y4pk6hn88z0bpny7a8k1919aqjzp0j1yhy9k1a"))

(define rust-fallible-streaming-iterator-0.1.9
  (crate-source "fallible-streaming-iterator" "0.1.9"
                "0nj6j26p71bjy8h42x6jahx1hn0ng6mc2miwpgwnp8vnwqf4jq3k"))

(define rust-fancy-regex-0.11.0
  (crate-source "fancy-regex" "0.11.0"
                "18j0mmzfycibhxhhhfja00dxd1vf8x5c28lbry224574h037qpxr"))

(define rust-fastrand-2.0.0
  (crate-source "fastrand" "2.0.0"
                "0r17m5p8ym5pa1f6cp8rix78ggclg6llnw5hxg168cr56wcdr6b9"))

(define rust-fastrand-2.3.0
  (crate-source "fastrand" "2.3.0"
                "1ghiahsw1jd68df895cy5h3gzwk30hndidn3b682zmshpgmrx41p"))

(define rust-fax-0.2.6
  (crate-source "fax" "0.2.6"
                "1ax0jmvsszxd03hj6ga1kyl7gaqcfw0akg2wf0q6gk9pizaffpgh"))

(define rust-fax-derive-0.2.0
  (crate-source "fax_derive" "0.2.0"
                "0zap434zz4xvi5rnysmwzzivig593b4ng15vwzwl7js2nw7s3b50"))

(define rust-fdeflate-0.3.7
  (crate-source "fdeflate" "0.3.7"
                "130ga18vyxbb5idbgi07njymdaavvk6j08yh1dfarm294ssm6s0y"))

(define rust-ff-0.13.0
  (crate-source "ff" "0.13.0"
                "0jcl8yhcs5kbfxfpnrhpkkvnk7s666vly6sgawg3nri9nx215m6y"))

(define rust-fiat-crypto-0.2.9
  (crate-source "fiat-crypto" "0.2.9"
                "07c1vknddv3ak7w89n85ik0g34nzzpms6yb845vrjnv9m4csbpi8"))

(define rust-field-offset-0.3.6
  (crate-source "field-offset" "0.3.6"
                "0zq5sssaa2ckmcmxxbly8qgz3sxpb8g1lwv90sdh1z74qif2gqiq"))

(define rust-file-size-1.0.3
  (crate-source "file-size" "1.0.3"
                "1cyj7067fs7ml8pjrwzjy3qrns3yxaxakf0na1v5fffk0l0z2i4m"))

(define rust-filetime-0.2.25
  (crate-source "filetime" "0.2.25"
                "11l5zr86n5sr6g6k6sqldswk0jzklm0q95rzikxcns0yk0p55h1m"))

(define rust-find-msvc-tools-0.1.2
  (crate-source "find-msvc-tools" "0.1.2"
                "0nbrhvk4m04hviiwbqp2jwcv9j2k70x0q2kcvfk51iygvaqp7v8w"))

(define rust-fixedbitset-0.4.2
  (crate-source "fixedbitset" "0.4.2"
                "101v41amgv5n9h4hcghvrbfk5vrncx1jwm35rn5szv4rk55i7rqc"))

(define rust-flate2-1.0.35
  (crate-source "flate2" "1.0.35"
                "0z6h0wa095wncpfngx75wyhyjnqwld7wax401gsvnzjhzgdbydn9"))

(define rust-flate2-1.1.2
  (crate-source "flate2" "1.1.2"
                "07abz7v50lkdr5fjw8zaw2v8gm2vbppc0f7nqm8x3v3gb6wpsgaa"))

(define rust-flex-grow-0.1.0
  (crate-source "flex-grow" "0.1.0"
                "0y7bm1bqa6cxwrj1w2l9ja9qcpaafmbgx28ildpnhq8mn0dsw16m"))

(define rust-float-cmp-0.9.0
  (crate-source "float-cmp" "0.9.0"
                "1i799ksbq7fj9rm9m82g1yqgm6xi3jnrmylddmqknmksajylpplq"))

(define rust-fnv-1.0.7
  (crate-source "fnv" "1.0.7"
                "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz"))

(define rust-foldhash-0.1.5
  (crate-source "foldhash" "0.1.5"
                "1wisr1xlc2bj7hk4rgkcjkz3j2x4dhd1h9lwk7mj8p71qpdgbi6r"))

(define rust-fontconfig-parser-0.5.8
  (crate-source "fontconfig-parser" "0.5.8"
                "0ijnbzg31sl6v49g7q2l7sl76hjj8z0hvlsz77cdvm029vi77ixv"))

(define rust-fontdb-0.23.0
  (crate-source "fontdb" "0.23.0"
                "0199vry9x8zn9ix4x4rqvv53dy2ryhy68l53jwr580hj7ndphzj5"))

(define rust-form-urlencoded-1.2.1
  (crate-source "form_urlencoded" "1.2.1"
                "0milh8x7nl4f450s3ddhg57a3flcv6yq8hlkyk6fyr3mcb128dp1"))

(define rust-form-urlencoded-1.2.2
  (crate-source "form_urlencoded" "1.2.2"
                "1kqzb2qn608rxl3dws04zahcklpplkd5r1vpabwga5l50d2v4k6b"))

(define rust-from-variants-0.6.0
  (crate-source "from_variants" "0.6.0"
                "1qx4jmwljwmcdfc998ndf7iz8wyg7lmlc3vl3fy812f9lfqiw6i2"))

(define rust-from-variants-impl-0.6.0
  (crate-source "from_variants_impl" "0.6.0"
                "06i4bjjxbq6c4hlx2ly04s64d1972zkskshc2v4xx7n8lfghf23y"))

(define rust-fs2-0.4.3
  (crate-source "fs2" "0.4.3"
                "04v2hwk7035c088f19mfl5b1lz84gnvv2hv6m935n0hmirszqr4m"))

(define rust-fsevent-sys-4.1.0
  ;; TODO: Check bundled sources.
  (crate-source "fsevent-sys" "4.1.0"
                "1liz67v8b0gcs8r31vxkvm2jzgl9p14i78yfqx81c8sdv817mvkn"))

(define rust-futures-0.3.30
  (crate-source "futures" "0.3.30"
                "1c04g14bccmprwsvx2j9m2blhwrynq7vhl151lsvcv4gi0b6jp34"))

(define rust-futures-0.3.31
  (crate-source "futures" "0.3.31"
                "0xh8ddbkm9jy8kc5gbvjp9a4b6rqqxvc8471yb2qaz5wm2qhgg35"))

(define rust-futures-channel-0.3.28
  (crate-source "futures-channel" "0.3.28"
                "1wmm9wm5zjigxz61qkscmxp7c30zp08dy63spjz5pch9gva1hmcm"))

(define rust-futures-channel-0.3.30
  (crate-source "futures-channel" "0.3.30"
                "0y6b7xxqdjm9hlcjpakcg41qfl7lihf6gavk8fyqijsxhvbzgj7a"))

(define rust-futures-channel-0.3.31
  (crate-source "futures-channel" "0.3.31"
                "040vpqpqlbk099razq8lyn74m0f161zd0rp36hciqrwcg2zibzrd"))

(define rust-futures-core-0.3.30
  (crate-source "futures-core" "0.3.30"
                "07aslayrn3lbggj54kci0ishmd1pr367fp7iks7adia1p05miinz"))

(define rust-futures-core-0.3.31
  (crate-source "futures-core" "0.3.31"
                "0gk6yrxgi5ihfanm2y431jadrll00n5ifhnpx090c2f2q1cr1wh5"))

(define rust-futures-executor-0.3.30
  (crate-source "futures-executor" "0.3.30"
                "07dh08gs9vfll2h36kq32q9xd86xm6lyl9xikmmwlkqnmrrgqxm5"))

(define rust-futures-executor-0.3.31
  (crate-source "futures-executor" "0.3.31"
                "17vcci6mdfzx4gbk0wx64chr2f13wwwpvyf3xd5fb1gmjzcx2a0y"))

(define rust-futures-io-0.3.30
  (crate-source "futures-io" "0.3.30"
                "1hgh25isvsr4ybibywhr4dpys8mjnscw4wfxxwca70cn1gi26im4"))

(define rust-futures-io-0.3.31
  (crate-source "futures-io" "0.3.31"
                "1ikmw1yfbgvsychmsihdkwa8a1knank2d9a8dk01mbjar9w1np4y"))

(define rust-futures-lite-2.5.0
  (crate-source "futures-lite" "2.5.0"
                "18cii1zgxbm04almisj0ycnmf7nj7qqyvy8x0i8mnl9cmqhhvx6f"))

(define rust-futures-macro-0.3.30
  (crate-source "futures-macro" "0.3.30"
                "1b49qh9d402y8nka4q6wvvj0c88qq91wbr192mdn5h54nzs0qxc7"))

(define rust-futures-macro-0.3.31
  (crate-source "futures-macro" "0.3.31"
                "0l1n7kqzwwmgiznn0ywdc5i24z72zvh9q1dwps54mimppi7f6bhn"))

(define rust-futures-sink-0.3.30
  (crate-source "futures-sink" "0.3.30"
                "1dag8xyyaya8n8mh8smx7x6w2dpmafg2din145v973a3hw7f1f4z"))

(define rust-futures-sink-0.3.31
  (crate-source "futures-sink" "0.3.31"
                "1xyly6naq6aqm52d5rh236snm08kw8zadydwqz8bip70s6vzlxg5"))

(define rust-futures-task-0.3.30
  (crate-source "futures-task" "0.3.30"
                "013h1724454hj8qczp8vvs10qfiqrxr937qsrv6rhii68ahlzn1q"))

(define rust-futures-task-0.3.31
  (crate-source "futures-task" "0.3.31"
                "124rv4n90f5xwfsm9qw6y99755y021cmi5dhzh253s920z77s3zr"))

(define rust-futures-util-0.3.30
  (crate-source "futures-util" "0.3.30"
                "0j0xqhcir1zf2dcbpd421kgw6wvsk0rpxflylcysn1rlp3g02r1x"))

(define rust-futures-util-0.3.31
  (crate-source "futures-util" "0.3.31"
                "10aa1ar8bgkgbr4wzxlidkqkcxf77gffyj8j7768h831pcaq784z"))

(define rust-fuzzy-matcher-0.3.7
  (crate-source "fuzzy-matcher" "0.3.7"
                "153csv8rsk2vxagb68kpmiknvdd3bzqj03x805khckck28rllqal"))

(define rust-fxhash-0.2.1
  (crate-source "fxhash" "0.2.1"
                "037mb9ichariqi45xm6mz0b11pa92gj38ba0409z3iz239sns6y3"))

(define rust-gdk-pixbuf-0.18.5
  (crate-source "gdk-pixbuf" "0.18.5"
                "1v7svvl0g7zybndmis5inaqqgi1mvcc6s1n8rkb31f5zn3qzbqah"))

(define rust-gdk-pixbuf-sys-0.18.0
  ;; TODO: Check bundled sources.
  (crate-source "gdk-pixbuf-sys" "0.18.0"
                "1xya543c4ffd2n7aiwwrdxsyc9casdbasafi6ixcknafckm3k61z"))

(define rust-gdk4-0.7.3
  (crate-source "gdk4" "0.7.3"
                "1xiacc63p73apr033gjrb9dsk0y4yxnsljwfxbwfry41snd03nvy"))

(define rust-gdk4-sys-0.7.2
  ;; TODO: Check bundled sources.
  (crate-source "gdk4-sys" "0.7.2"
                "1w7yvir565sjrrw828lss07749hfpfsr19jdjzwivkx36brl7ayv"))

(define rust-generic-array-0.14.7
  (crate-source "generic-array" "0.14.7"
                "16lyyrzrljfq424c3n8kfwkqihlimmsg5nhshbbp48np3yjrqr45"))

(define rust-gethostname-0.3.0
  (crate-source "gethostname" "0.3.0"
                "0xy1wbx5k2bzi4cbaqj9wqgqsbn4f8pm6nsm1d86mibk66xd8rdv"))

(define rust-gethostname-0.4.3
  (crate-source "gethostname" "0.4.3"
                "063qqhznyckwx9n4z4xrmdv10s0fi6kbr17r6bi1yjifki2y0xh1"))

(define rust-getrandom-0.2.10
  (crate-source "getrandom" "0.2.10"
                "09zlimhhskzf7cmgcszix05wyz2i6fcpvh711cv1klsxl6r3chdy"))

(define rust-getrandom-0.2.12
  (crate-source "getrandom" "0.2.12"
                "1d8jb9bv38nkwlqqdjcav6gxckgwc9g30pm3qq506rvncpm9400r"))

(define rust-getrandom-0.2.15
  (crate-source "getrandom" "0.2.15"
                "1mzlnrb3dgyd1fb84gvw10pyr8wdqdl4ry4sr64i1s8an66pqmn4"))

(define rust-getrandom-0.2.16
  (crate-source "getrandom" "0.2.16"
                "14l5aaia20cc6cc08xdlhrzmfcylmrnprwnna20lqf746pqzjprk"))

(define rust-getrandom-0.3.3
  (crate-source "getrandom" "0.3.3"
                "1x6jl875zp6b2b6qp9ghc84b0l76bvng2lvm8zfcmwjl7rb5w516"))

(define rust-gettext-rs-0.7.0
  (crate-source "gettext-rs" "0.7.0"
                "0r7kahqcjrkm83d3gzzkn83fnw2bnqj2ank5z6hsm66izalai7p4"))

(define rust-gettext-sys-0.21.3
  ;; TODO: Check bundled sources.
  (crate-source "gettext-sys" "0.21.3"
                "17c3qdbirxsf9csqzp4z4jaqck2n72z4nw3nh9vhd8jn1zhf4g66"))

(define rust-gif-0.13.3
  (crate-source "gif" "0.13.3"
                "06z6gll24q7psbz9fb86jbcbmgwnxkym8jsp0fbq5qikbqilgq2a"))

(define rust-gimli-0.28.0
  (crate-source "gimli" "0.28.0"
                "1h7hcl3chfvd2gfrrxjymnwj7anqxjslvz20kcargkvsya2dgf3g"))

(define rust-gimli-0.28.1
  (crate-source "gimli" "0.28.1"
                "0lv23wc8rxvmjia3mcxc6hj9vkqnv1bqq0h8nzjcgf71mrxx6wa2"))

(define rust-gio-0.18.4
  (crate-source "gio" "0.18.4"
                "0wsc6mnx057s4ailacg99dwgna38dbqli5x7a6y9rdw75x9qzz6l"))

(define rust-gio-sys-0.18.1
  ;; TODO: Check bundled sources.
  (crate-source "gio-sys" "0.18.1"
                "1lip8z35iy9d184x2qwjxlbxi64q9cpayy7v1p5y9xdsa3w6smip"))

(define rust-git2-0.19.0
  (crate-source "git2" "0.19.0"
                "091pv7866z1qjq800ys0wjv8n73wrv7fqdrddxcnq36w8lzbf0xr"))

(define rust-git2-0.20.2
  (crate-source "git2" "0.20.2"
                "0451zzmvblvlrj6y6pgdsxrqh42hi789n3k9lp0hslmi6fhhgsrd"))

(define rust-glassbench-0.4.4
  (crate-source "glassbench" "0.4.4"
                "0459fsmfhdvg2z1085a6gnydl1y8s3xanrr9l3vqqd0s51q7hqsa"))

(define rust-glib-0.18.5
  (crate-source "glib" "0.18.5"
                "1r8fw0627nmn19bgk3xpmcfngx3wkn7mcpq5a8ma3risx3valg93"))

(define rust-glib-macros-0.18.5
  (crate-source "glib-macros" "0.18.5"
                "1p5cla53fcp195zp0hkqpmnn7iwmkdswhy7xh34002bw8y7j5c0b"))

(define rust-glib-sys-0.18.1
  ;; TODO: Check bundled sources.
  (crate-source "glib-sys" "0.18.1"
                "164qhsfmlzd5mhyxs8123jzbdfldwxbikfpq5cysj3lddbmy4g06"))

(define rust-glob-0.3.3
  (crate-source "glob" "0.3.3"
                "106jpd3syfzjfj2k70mwm0v436qbx96wig98m4q8x071yrq35hhc"))

(define rust-gobject-sys-0.18.0
  ;; TODO: Check bundled sources.
  (crate-source "gobject-sys" "0.18.0"
                "0i6fhp3m6vs3wkzyc22rk2cqj68qvgddxmpaai34l72da5xi4l08"))

(define rust-gpg-error-0.6.2
  (crate-source "gpg-error" "0.6.2"
                "06frmsf3sn286snvwxlwq5jyfvg8wq2661y877b38mz9s0aawnjl"))

(define rust-gpgme-0.11.0
  (crate-source "gpgme" "0.11.0"
                "0ky4b816g1c7f4r82jbsmfivrl0f8wmvfd27k3dsr3p5zcr9flsp"))

(define rust-gpgme-sys-0.11.0
  ;; TODO: Check bundled sources.
  (crate-source "gpgme-sys" "0.11.0"
                "1xqwfilq63nmxk38cz4i64nz08vpj6ndcdwl48k4lvn0b7b274jh"))

(define rust-graphene-rs-0.18.1
  (crate-source "graphene-rs" "0.18.1"
                "00f4q1ra4haap5i7lazwhkdgnb49fs8adk2nm6ki6mjhl76jh8iv"))

(define rust-graphene-sys-0.18.1
  ;; TODO: Check bundled sources.
  (crate-source "graphene-sys" "0.18.1"
                "0n8zlg7z26lwpnvlqp1hjlgrs671skqwagdpm7r8i1zwx3748hfc"))

(define rust-group-0.13.0
  (crate-source "group" "0.13.0"
                "0qqs2p5vqnv3zvq9mfjkmw3qlvgqb0c3cm6p33srkh7pc9sfzygh"))

(define rust-gsk4-0.7.3
  (crate-source "gsk4" "0.7.3"
                "0zhzs2dkgiinhgc11akpn2harq3x5n1iq21dnc4h689g3lsqx58d"))

(define rust-gsk4-sys-0.7.3
  ;; TODO: Check bundled sources.
  (crate-source "gsk4-sys" "0.7.3"
                "0mbdlm9qi1hql48rr29vsj9vlqwc7gxg67wg1q19z67azwz9xg8j"))

(define rust-gtk4-0.7.3
  (crate-source "gtk4" "0.7.3"
                "0hh8nzglmz94v1m1h6vy8z12m6fr7ia467ry0md5fa4p7sm53sss"))

(define rust-gtk4-macros-0.7.2
  (crate-source "gtk4-macros" "0.7.2"
                "0bw3cchiycf7dw1bw4p8946gv38azxy05a5w0ndgcmxnz6fc8znm"))

(define rust-gtk4-sys-0.7.3
  ;; TODO: Check bundled sources.
  (crate-source "gtk4-sys" "0.7.3"
                "1f2ylskyqkjdik9fij2m46pra4jagnif5xyalbxfk3334fmc9n2l"))

(define rust-half-2.6.0
  (crate-source "half" "2.6.0"
                "1j83v0xaqvrw50ppn0g33zig0zsbdi7xiqbzgn7sd5al57nrd4a5"))

(define rust-hashbrown-0.14.3
  (crate-source "hashbrown" "0.14.3"
                "012nywlg0lj9kwanh69my5x67vjlfmzfi9a0rq4qvis2j8fil3r9"))

(define rust-hashbrown-0.14.5
  (crate-source "hashbrown" "0.14.5"
                "1wa1vy1xs3mp11bn3z9dv0jricgr6a2j0zkf1g19yz3vw4il89z5"))

(define rust-hashbrown-0.15.2
  (crate-source "hashbrown" "0.15.2"
                "12dj0yfn59p3kh3679ac0w1fagvzf4z2zp87a13gbbqbzw0185dz"))

(define rust-hashbrown-0.15.5
  (crate-source "hashbrown" "0.15.5"
                "189qaczmjxnikm9db748xyhiw04kpmhm9xj9k9hg0sgx7pjwyacj"))

(define rust-hashbrown-0.16.0
  (crate-source "hashbrown" "0.16.0"
                "13blh9j2yv77a6ni236ixiwdzbc1sh2bc4bdpaz7y859yv2bs6al"))

(define rust-hashlink-0.9.1
  (crate-source "hashlink" "0.9.1"
                "1byq4nyrflm5s6wdx5qwp96l1qbp2d0nljvrr5yqrsfy51qzz93b"))

(define rust-heck-0.4.1
  (crate-source "heck" "0.4.1"
                "1a7mqsnycv5z4z5vnv1k34548jzmc0ajic7c1j8jsaspnhw5ql4m"))

(define rust-heck-0.5.0
  (crate-source "heck" "0.5.0"
                "1sjmpsdl8czyh9ywl3qcsfsq9a307dg4ni2vnlwgnzzqhc4y0113"))

(define rust-hermit-abi-0.3.5
  (crate-source "hermit-abi" "0.3.5"
                "1hw2bxkzyvr0rbnpj0lkasi8h8qf3lyb63hp760cn22fjqaj3inh"))

(define rust-hermit-abi-0.3.9
  (crate-source "hermit-abi" "0.3.9"
                "092hxjbjnq5fmz66grd9plxd0sh6ssg5fhgwwwqbrzgzkjwdycfj"))

(define rust-hermit-abi-0.4.0
  (crate-source "hermit-abi" "0.4.0"
                "1k1zwllx6nfq417hy38x4akw1ivlv68ymvnzyxs76ffgsqcskxpv"))

(define rust-hex-0.4.3
  (crate-source "hex" "0.4.3"
                "0w1a4davm1lgzpamwnba907aysmlrnygbqmfis2mqjx5m552a93z"))

(define rust-hickory-proto-0.24.2
  (crate-source "hickory-proto" "0.24.2"
                "1df1gg333sgjicmf8nbsxlhffp18kdfwcvdgaald1fdgp36zsyj4"))

(define rust-hickory-resolver-0.24.2
  (crate-source "hickory-resolver" "0.24.2"
                "1d243gxb2c2nlm73c2vb4yp908z8mi6ixkriglkfb71qkjx2lbha"))

(define rust-hkdf-0.12.4
  (crate-source "hkdf" "0.12.4"
                "1xxxzcarz151p1b858yn5skmhyrvn8fs4ivx5km3i1kjmnr8wpvv"))

(define rust-hmac-0.12.1
  (crate-source "hmac" "0.12.1"
                "0pmbr069sfg76z7wsssfk5ddcqd9ncp79fyz6zcm6yn115yc6jbc"))

(define rust-home-0.5.11
  (crate-source "home" "0.5.11"
                "1kxb4k87a9sayr8jipr7nq9wpgmjk4hk4047hmf9kc24692k75aq"))

(define rust-hostname-0.3.1
  (crate-source "hostname" "0.3.1"
                "0rz8yf70cvzl3nry71m4bz9w6x4j9kdz3qng6pnwhk2h20z1qwrw"))

(define rust-http-1.1.0
  (crate-source "http" "1.1.0"
                "0n426lmcxas6h75c2cp25m933pswlrfjz10v91vc62vib2sdvf91"))

(define rust-http-1.2.0
  (crate-source "http" "1.2.0"
                "1skglzdf98j5nzxlii540n11is0w4l80mi5sm3xrj716asps4v7i"))

(define rust-http-body-1.0.1
  (crate-source "http-body" "1.0.1"
                "111ir5k2b9ihz5nr9cz7cwm7fnydca7dx4hc7vr16scfzghxrzhy"))

(define rust-http-body-util-0.1.2
  (crate-source "http-body-util" "0.1.2"
                "0kslwazg4400qnc2azkrgqqci0fppv12waicnsy5d8hncvbjjd3r"))

(define rust-http-lib-0.1.0
  (crate-source "http-lib" "0.1.0"
                "0f0klsaiczfw3rkj5h8zncwg70qkwjqxkjfi45ccnl85r4ivsk4r"))

(define rust-httparse-1.8.0
  (crate-source "httparse" "1.8.0"
                "010rrfahm1jss3p022fqf3j3jmm72vhn4iqhykahb9ynpaag75yq"))

(define rust-httparse-1.9.5
  (crate-source "httparse" "1.9.5"
                "0ip9v8m9lvgvq1lznl31wvn0ch1v254na7lhid9p29yx9rbx6wbx"))

(define rust-httpdate-1.0.3
  (crate-source "httpdate" "1.0.3"
                "1aa9rd2sac0zhjqh24c9xvir96g188zldkx0hr6dnnlx5904cfyz"))

(define rust-hyper-1.4.1
  (crate-source "hyper" "1.4.1"
                "01ds8i3q6hw5kw56mavy544m11gkr87zi999siigdl3n1qpd5psh"))

(define rust-hyper-rustls-0.26.0
  (crate-source "hyper-rustls" "0.26.0"
                "0b4m1jvs147hxi8677n2dxxib663s7c31xmfni7b5qkanihsggm0"))

(define rust-hyper-util-0.1.7
  (crate-source "hyper-util" "0.1.7"
                "1fg9h591skksq5zxnffyisj7487jhdcgj6c7bvlkckn535bhbryd"))

(define rust-hyperlocal-0.9.1
  (crate-source "hyperlocal" "0.9.1"
                "1iy8rhsap5iyigj6s86nk449zl5bahjycy2mswy6nlllp7imqv4q"))

(define rust-iana-time-zone-0.1.61
  (crate-source "iana-time-zone" "0.1.61"
                "085jjsls330yj1fnwykfzmb2f10zp6l7w4fhq81ng81574ghhpi3"))

(define rust-iana-time-zone-0.1.64
  (crate-source "iana-time-zone" "0.1.64"
                "1yz980fmhaq9bdkasz35z63az37ci6kzzfhya83kgdqba61pzr9k"))

(define rust-iana-time-zone-haiku-0.1.2
  (crate-source "iana-time-zone-haiku" "0.1.2"
                "17r6jmj31chn7xs9698r122mapq85mfnv98bb4pg6spm0si2f67k"))

(define rust-icu-collections-1.5.0
  (crate-source "icu_collections" "1.5.0"
                "09j5kskirl59mvqc8kabhy7005yyy7dp88jw9f6f3gkf419a8byv"))

(define rust-icu-collections-2.0.0
  (crate-source "icu_collections" "2.0.0"
                "0izfgypv1hsxlz1h8fc2aak641iyvkak16aaz5b4aqg3s3sp4010"))

(define rust-icu-locale-core-2.0.0
  (crate-source "icu_locale_core" "2.0.0"
                "02phv7vwhyx6vmaqgwkh2p4kc2kciykv2px6g4h8glxfrh02gphc"))

(define rust-icu-locid-1.5.0
  (crate-source "icu_locid" "1.5.0"
                "0dznvd1c5b02iilqm044q4hvar0sqibq1z46prqwjzwif61vpb0k"))

(define rust-icu-locid-transform-1.5.0
  (crate-source "icu_locid_transform" "1.5.0"
                "0kmmi1kmj9yph6mdgkc7v3wz6995v7ly3n80vbg0zr78bp1iml81"))

(define rust-icu-locid-transform-data-1.5.0
  (crate-source "icu_locid_transform_data" "1.5.0"
                "0vkgjixm0wzp2n3v5mw4j89ly05bg3lx96jpdggbwlpqi0rzzj7x"))

(define rust-icu-normalizer-1.5.0
  (crate-source "icu_normalizer" "1.5.0"
                "0kx8qryp8ma8fw1vijbgbnf7zz9f2j4d14rw36fmjs7cl86kxkhr"))

(define rust-icu-normalizer-2.0.0
  (crate-source "icu_normalizer" "2.0.0"
                "0ybrnfnxx4sf09gsrxri8p48qifn54il6n3dq2xxgx4dw7l80s23"))

(define rust-icu-normalizer-data-1.5.0
  (crate-source "icu_normalizer_data" "1.5.0"
                "05lmk0zf0q7nzjnj5kbmsigj3qgr0rwicnn5pqi9n7krmbvzpjpq"))

(define rust-icu-normalizer-data-2.0.0
  (crate-source "icu_normalizer_data" "2.0.0"
                "1lvjpzxndyhhjyzd1f6vi961gvzhj244nribfpdqxjdgjdl0s880"))

(define rust-icu-properties-1.5.1
  (crate-source "icu_properties" "1.5.1"
                "1xgf584rx10xc1p7zjr78k0n4zn3g23rrg6v2ln31ingcq3h5mlk"))

(define rust-icu-properties-2.0.1
  (crate-source "icu_properties" "2.0.1"
                "0az349pjg8f18lrjbdmxcpg676a7iz2ibc09d2wfz57b3sf62v01"))

(define rust-icu-properties-data-1.5.0
  (crate-source "icu_properties_data" "1.5.0"
                "0scms7pd5a7yxx9hfl167f5qdf44as6r3bd8myhlngnxqgxyza37"))

(define rust-icu-properties-data-2.0.1
  (crate-source "icu_properties_data" "2.0.1"
                "0cnn3fkq6k88w7p86w7hsd1254s4sl783rpz4p6hlccq74a5k119"))

(define rust-icu-provider-1.5.0
  (crate-source "icu_provider" "1.5.0"
                "1nb8vvgw8dv2inqklvk05fs0qxzkw8xrg2n9vgid6y7gm3423m3f"))

(define rust-icu-provider-2.0.0
  (crate-source "icu_provider" "2.0.0"
                "1bz5v02gxv1i06yhdhs2kbwxkw3ny9r2vvj9j288fhazgfi0vj03"))

(define rust-icu-provider-macros-1.5.0
  (crate-source "icu_provider_macros" "1.5.0"
                "1mjs0w7fcm2lcqmbakhninzrjwqs485lkps4hz0cv3k36y9rxj0y"))

(define rust-id-arena-2.2.1
  (crate-source "id-arena" "2.2.1"
                "01ch8jhpgnih8sawqs44fqsqpc7bzwgy0xpi6j0f4j0i5mkvr8i5"))

(define rust-idea-0.5.1
  (crate-source "idea" "0.5.1"
                "0xv4hd9mgrwgzfl7cc5nlwyahm9yni5z9dwb3c1z5mqr8h05fm87"))

(define rust-ident-case-1.0.1
  (crate-source "ident_case" "1.0.1"
                "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r"))

(define rust-idna-1.0.3
  (crate-source "idna" "1.0.3"
                "0zlajvm2k3wy0ay8plr07w22hxkkmrxkffa6ah57ac6nci984vv8"))

(define rust-idna-1.1.0
  (crate-source "idna" "1.1.0"
                "1pp4n7hppm480zcx411dsv9wfibai00wbpgnjj4qj0xa7kr7a21v"))

(define rust-idna-adapter-1.2.0
  (crate-source "idna_adapter" "1.2.0"
                "0wggnkiivaj5lw0g0384ql2d7zk4ppkn3b1ry4n0ncjpr7qivjns"))

(define rust-idna-adapter-1.2.1
  (crate-source "idna_adapter" "1.2.1"
                "0i0339pxig6mv786nkqcxnwqa87v4m94b2653f6k3aj0jmhfkjis"))

(define rust-image-0.25.8
  (crate-source "image" "0.25.8"
                "1rwill018gn2kwzv332kfs72ns0kwwnfxwacbhvk9lk9cwzfp7sj"))

(define rust-image-webp-0.2.4
  (crate-source "image-webp" "0.2.4"
                "1hz814csyi9283vinzlkix6qpnd6hs3fkw7xl6z2zgm4w7rrypjj"))

(define rust-imagesize-0.13.0
  (crate-source "imagesize" "0.13.0"
                "11f26ac9zvbr7sjnsv2z9jd3ryaz40pg8xch4ij1q1rg5zbjgkgd"))

(define rust-imap-client-0.2.3
  (crate-source "imap-client" "0.2.3"
                "0812nlk2sadi5k9c74n5mksb73plxcr7rjk4gnd8xi460rabv0g7"))

(define rust-imap-codec-2.0.0-alpha.5
  (crate-source "imap-codec" "2.0.0-alpha.5"
                "0mw5n97wskdf861032x63sn694pckwkprw74i3i8zyyivl53317m"))

(define rust-imap-next-0.3.1
  (crate-source "imap-next" "0.3.1"
                "0hwvdga7rg32wnfqznshhv80wmkghzhgk7dzxpcyina2wwh5bn2n"))

(define rust-imap-types-2.0.0-alpha.4
  (crate-source "imap-types" "2.0.0-alpha.4"
                "0lzgr4lihafycakpiadlch1y6w0p67bmllrdrjd68aln24gxh0fn"))

(define rust-imgref-1.12.0
  (crate-source "imgref" "1.12.0"
                "1j3iwdal9mdkmyrsms3lz4n1bxxxjxss2jvbmh662fns63fcxig7"))

(define rust-include-dir-0.7.4
  (crate-source "include_dir" "0.7.4"
                "1pfh3g45z88kwq93skng0n6g3r7zkhq9ldqs9y8rvr7i11s12gcj"))

(define rust-include-dir-macros-0.7.4
  (crate-source "include_dir_macros" "0.7.4"
                "0x8smnf6knd86g69p19z5lpfsaqp8w0nx14kdpkz1m8bxnkqbavw"))

(define rust-indenter-0.3.3
  (crate-source "indenter" "0.3.3"
                "10y6i6y4ls7xsfsc1r3p5j2hhbxhaqnk5zzk8aj52b14v05ba8yf"))

(define rust-indexmap-2.11.4
  (crate-source "indexmap" "2.11.4"
                "1rc8bgcjzfcskz1zipjjm7s3m1jskzhnhr9jxmsafhdk1xv863sb"))

(define rust-indexmap-2.2.2
  (crate-source "indexmap" "2.2.2"
                "087mafd9f98rp1xk2jc1rsp5yyqz63yi30cy8yx6c8s14bj2ljw2"))

(define rust-indexmap-2.7.0
  (crate-source "indexmap" "2.7.0"
                "07s7jmdymvd0rm4yswp0j3napx57hkjm9gs9n55lvs2g78vj5y32"))

(define rust-inflector-0.11.4
  (crate-source "Inflector" "0.11.4"
                "1lqmcni21ifzyq41fhz6k1j2b23cmsx469s4g4sf01l78miqqhzy"))

(define rust-inotify-0.11.0
  (crate-source "inotify" "0.11.0"
                "1wq8m657rl085cg59p38sc5y62xy9yhhpvxbkd7n1awi4zzwqzgk"))

(define rust-inotify-0.9.6
  (crate-source "inotify" "0.9.6"
                "1zxb04c4qccp8wnr3v04l503qpxzxzzzph61amlqbsslq4z9s1pq"))

(define rust-inotify-sys-0.1.5
  ;; TODO: Check bundled sources.
  (crate-source "inotify-sys" "0.1.5"
                "1syhjgvkram88my04kv03s0zwa66mdwa5v7ddja3pzwvx2sh4p70"))

(define rust-inout-0.1.3
  (crate-source "inout" "0.1.3"
                "1xf9gf09nc7y1a261xlfqsf66yn6mb81ahlzzyyd1934sr9hbhd0"))

(define rust-inquire-0.7.5
  (crate-source "inquire" "0.7.5"
                "0an1bjs0yklk990d7ni65znmirs99p01wld221affw5g64qgkp8g"))

(define rust-instant-0.1.13
  (crate-source "instant" "0.1.13"
                "08h27kzvb5jw74mh0ajv0nv9ggwvgqm8ynjsn2sa9jsks4cjh970"))

(define rust-interpolate-name-0.2.4
  (crate-source "interpolate_name" "0.2.4"
                "0q7s5mrfkx4p56dl8q9zq71y1ysdj4shh6f28qf9gly35l21jj63"))

(define rust-io-kit-sys-0.4.1
  ;; TODO: Check bundled sources.
  (crate-source "io-kit-sys" "0.4.1"
                "0ysy5k3wf54yangy25hkj10xx332cj2hb937xasg6riziv7yczk1"))

(define rust-ipconfig-0.3.2
  (crate-source "ipconfig" "0.3.2"
                "0zwr0x3jnqmjdqqbzhb0nid011qyhcyfdfqv32cdw85pjqpvk3dm"))

(define rust-ipnet-2.10.1
  (crate-source "ipnet" "2.10.1"
                "025p9wm94q1w2l13hbbr4cbmfygly3a2ag8g5s618l2jhq4l3hnx"))

(define rust-is-docker-0.2.0
  (crate-source "is-docker" "0.2.0"
                "1cyibrv6817cqcpf391m327ss40xlbik8wxcv5h9pj9byhksx2wj"))

(define rust-is-executable-1.0.5
  (crate-source "is_executable" "1.0.5"
                "1i78ss45h94nwabbn6ki64a91djlli8zdwwbh56jj9kvhssbiaxs"))

(define rust-is-terminal-polyfill-1.70.1
  (crate-source "is_terminal_polyfill" "1.70.1"
                "1kwfgglh91z33kl0w5i338mfpa3zs0hidq5j4ny4rmjwrikchhvr"))

(define rust-is-wsl-0.4.0
  (crate-source "is-wsl" "0.4.0"
                "19bs5pq221d4bknnwiqqkqrnsx2in0fsk8fylxm1747iim4hjdhp"))

(define rust-itertools-0.12.1
  (crate-source "itertools" "0.12.1"
                "0s95jbb3ndj1lvfxyq5wanc0fm0r6hg6q4ngb92qlfdxvci10ads"))

(define rust-itoa-1.0.10
  (crate-source "itoa" "1.0.10"
                "0k7xjfki7mnv6yzjrbnbnjllg86acmbnk4izz2jmm1hx2wd6v95i"))

(define rust-itoa-1.0.14
  (crate-source "itoa" "1.0.14"
                "0x26kr9m062mafaxgcf2p6h2x7cmixm0zw95aipzn2hr3d5jlnnp"))

(define rust-itoa-1.0.15
  (crate-source "itoa" "1.0.15"
                "0b4fj9kz54dr3wam0vprjwgygvycyw8r0qwg7vp19ly8b2w16psa"))

(define rust-itoa-1.0.9
  (crate-source "itoa" "1.0.9"
                "0f6cpb4yqzhkrhhg6kqsw3wnmmhdnnffi6r2xzy248gzi2v0l5dg"))

(define rust-jni-0.19.0
  (crate-source "jni" "0.19.0"
                "1v0pn0i1wb8zp4wns4l8hz9689hqsplv7iba7hylaznvwg11ipy6"))

(define rust-jni-sys-0.3.0
  ;; TODO: Check bundled sources.
  (crate-source "jni-sys" "0.3.0"
                "0c01zb9ygvwg9wdx2fii2d39myzprnpqqhy7yizxvjqp5p04pbwf"))

(define rust-jobserver-0.1.32
  (crate-source "jobserver" "0.1.32"
                "1l2k50qmj84x9mn39ivjz76alqmx72jhm12rw33zx9xnpv5xpla8"))

(define rust-jobserver-0.1.34
  (crate-source "jobserver" "0.1.34"
                "0cwx0fllqzdycqn4d6nb277qx5qwnmjdxdl0lxkkwssx77j3vyws"))

(define rust-jpeg-encoder-0.5.1
  (crate-source "jpeg-encoder" "0.5.1"
                "01j9wslaqs0ragv3i5xkkjz2l8lbarspbv4h4scz3ngz4zzazwrc"))

(define rust-js-sys-0.3.72
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.72"
                "1a8r61hbgw5kmscgj3g5pzg2ywlnswvljy0l592v0xdxlayz323a"))

(define rust-js-sys-0.3.76
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.76"
                "1dz7v777h2j38wkf8k5iwkfxskn6nff2cdv2jsslyxkpn2svc5v7"))

(define rust-js-sys-0.3.81
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.81"
                "01ckbf16iwh7qj92fax9zh8vf2y9sk60cli6999cn7a1jxx96j7c"))

(define rust-jxl-bitstream-0.2.3
  (crate-source "jxl-bitstream" "0.2.3"
                "08k6dy6v5qwlaplc02vln2pzfvw8jbjndpllysrjjqng31fj1hvn"))

(define rust-jxl-coding-0.2.3
  (crate-source "jxl-coding" "0.2.3"
                "1d6h5ms7ffclrag48892zq0i9byjvsl3zg0zvpifj71cfq660xbh"))

(define rust-jxl-color-0.3.2
  (crate-source "jxl-color" "0.3.2"
                "0wvfiyqf0pmm189g6d83g3iz8mqslla7a63cvfqjc7nhnqrvr14h"))

(define rust-jxl-frame-0.5.0
  (crate-source "jxl-frame" "0.5.0"
                "1qa5bvkxsrzqlhb5z9n5r7ymjpda83wqdpi3l5lncx730k8vsqwd"))

(define rust-jxl-grid-0.1.1
  (crate-source "jxl-grid" "0.1.1"
                "1s2547v112wvljkxamqz44dgpg9h0pa9rbhqz31bpcvbxlhhp028"))

(define rust-jxl-image-0.5.0
  (crate-source "jxl-image" "0.5.0"
                "0ndyc28vppfrdg9dg8sy8l65mvyd007073acc1k9x76c9bvzg1pg"))

(define rust-jxl-modular-0.3.0
  (crate-source "jxl-modular" "0.3.0"
                "0j377r653p32zxlyfymndz2qcrzwfb3r66g85dcnh99nvdannkjh"))

(define rust-jxl-oxide-0.4.0
  (crate-source "jxl-oxide" "0.4.0"
                "0r30kcp0hfs4xab4bwrp8haxp08m757mijd09jf9f8yqb7jbgqsp"))

(define rust-jxl-render-0.4.0
  (crate-source "jxl-render" "0.4.0"
                "0zkhr03nckj8d6gg6lgwkamifx5s8njqpk5h1j0dqvc9qk3d2mvi"))

(define rust-jxl-vardct-0.3.0
  (crate-source "jxl-vardct" "0.3.0"
                "0gh52l6na3yn9kwajfdrr44i6zf6m8w1r81v2kv552n4m2djsjpb"))

(define rust-kamadak-exif-0.5.5
  (crate-source "kamadak-exif" "0.5.5"
                "0xw0lpmra8j1y98c0agwrmjajpkh91mnl89hzaxbdrdp186wfkzg"))

(define rust-keccak-0.1.5
  (crate-source "keccak" "0.1.5"
                "0m06swsyd58hvb1z17q6picdwywprf1yf1s6l491zi8r26dazhpc"))

(define rust-keyring-3.6.1
  (crate-source "keyring" "3.6.1"
                "0cq8y72lwpl2n4n2hxz8lxwsz5in0q8fkmsnswvlnkjd8qwyi3rg"))

(define rust-keyring-lib-1.0.2
  (crate-source "keyring-lib" "1.0.2"
                "062jjqr9l7xs8cxnb6cvns32mkb4cyvqcyq4qr8kvwv5qic1b4jn"))

(define rust-kqueue-1.0.8
  (crate-source "kqueue" "1.0.8"
                "033x2knkbv8d3jy6i9r32jcgsq6zm3g97zh5la43amkv3g5g2ivl"))

(define rust-kqueue-1.1.1
  (crate-source "kqueue" "1.1.1"
                "0sjrsnza8zxr1zfpv6sa0zapd54kx9wlijrz9apqvs6wsw303hza"))

(define rust-kqueue-sys-1.0.4
  ;; TODO: Check bundled sources.
  (crate-source "kqueue-sys" "1.0.4"
                "12w3wi90y4kwis4k9g6fp0kqjdmc6l00j16g8mgbhac7vbzjb5pd"))

(define rust-kurbo-0.11.3
  (crate-source "kurbo" "0.11.3"
                "0qiwq4l83fy9v5d1piywvh44yg9ha3rl04d2kdcqlvvm8jp2c866"))

(define rust-lazy-regex-3.4.1
  (crate-source "lazy-regex" "3.4.1"
                "09kiz9pmpfj8ddfr6kfrxg5kqinkk3llvsm7iywkcav8jc5k3iv0"))

(define rust-lazy-regex-proc-macros-3.4.1
  (crate-source "lazy-regex-proc_macros" "3.4.1"
                "1886sbdzfsqmgnyzsw1hmjdy58xks44j23sy1aqpxqc1xysiv82b"))

(define rust-lazy-static-1.4.0
  (crate-source "lazy_static" "1.4.0"
                "0in6ikhw8mgl33wjv6q6xfrb5b9jr16q8ygjy803fay4zcisvaz2"))

(define rust-lazy-static-1.5.0
  (crate-source "lazy_static" "1.5.0"
                "1zk6dqqni0193xg6iijh7i3i44sryglwgvx20spdvwk3r6sbrlmv"))

(define rust-lebe-0.5.3
  (crate-source "lebe" "0.5.3"
                "1f459clndzzm35nyd15vj5dlasyagfasp7hcgl6lh2b658rs6ybs"))

(define rust-lexopt-0.3.0
  (crate-source "lexopt" "0.3.0"
                "00dlvik2ygw8z101vf3bfndcvxhp92v25sbzz6bdiwvxgxhlpzxs"))

(define rust-lfs-core-0.14.1
  (crate-source "lfs-core" "0.14.1"
                "0sga4zdsal26s3fpdizgpakgzd80afbcfz07wsr2bczmqvgb4pdn"))

(define rust-libadwaita-0.5.3
  (crate-source "libadwaita" "0.5.3"
                "174pzn9dwsk8ikvrhx13vkh0zrpvb3rhg9yd2q5d2zjh0q6fgrrg"))

(define rust-libadwaita-sys-0.5.3
  ;; TODO: Check bundled sources.
  (crate-source "libadwaita-sys" "0.5.3"
                "16n6xsy6jhbj0jbpz8yvql6c9b89a99v9vhdz5s37mg1inisl42y"))

(define rust-libc-0.2.153
  (crate-source "libc" "0.2.153"
                "1gg7m1ils5dms5miq9fyllrcp0jxnbpgkx71chd2i0lafa8qy6cw"))

(define rust-libc-0.2.161
  (crate-source "libc" "0.2.161"
                "1lc5s3zd0491x9zxrv2kvclai1my1spz950pkkyry4vwh318k54f"))

(define rust-libc-0.2.169
  (crate-source "libc" "0.2.169"
                "02m253hs8gw0m1n8iyrsc4n15yzbqwhddi7w1l0ds7i92kdsiaxm"))

(define rust-libc-0.2.176
  (crate-source "libc" "0.2.176"
                "0x7ivn80h7nz2l46vra7bxx36s6r8d0lkax14dx97skjsss2kyaq"))

(define rust-libdav-0.9.1
  (crate-source "libdav" "0.9.1"
                "1646mcnalav3jiprn3xyslyncmcvn34jzw5qn0h4k1x0bppczqhm"))

(define rust-libdbus-sys-0.2.5
  ;; TODO: Check bundled sources.
  (crate-source "libdbus-sys" "0.2.5"
                "0wjw93q6ckrn8qdrxzdi02f0ma9g7nnlpgkrkcll1mjhnw95a206"))

(define rust-libfuzzer-sys-0.4.10
  ;; TODO: Check bundled sources.
  (crate-source "libfuzzer-sys" "0.4.10"
                "0124z86582vyzl8gbadqscjgf9i94jcpa9mxcpsyxjvh3w71jdsh"))

(define rust-libgit2-sys-0.17.0+1.8.1
  ;; TODO: Check bundled sources.
  (crate-source "libgit2-sys" "0.17.0+1.8.1"
                "093jxfl2i9vxdlgf7vk9d040sjwy0nq4fid640y7qix6m0k26iqh"))

(define rust-libgit2-sys-0.18.2+1.9.1
  ;; TODO: Check bundled sources.
  (crate-source "libgit2-sys" "0.18.2+1.9.1"
                "08n223x2pkf4gj6yrjmh3z6q236qj6nifwww78xcblrbvw1zwhhw"))

(define rust-libgpg-error-sys-0.6.2
  ;; TODO: Check bundled sources.
  (crate-source "libgpg-error-sys" "0.6.2"
                "10kcif9qc4f6f52gdh5bvszg8vfxfjga2wygbc585v8n12y4q2jh"))

(define rust-libm-0.2.11
  (crate-source "libm" "0.2.11"
                "1yjgk18rk71rjbqcw9l1zaqna89p9s603k7n327nqs8dn88vwmc3"))

(define rust-libm-0.2.15
  (crate-source "libm" "0.2.15"
                "1plpzf0p829viazdj57yw5dhmlr8ywf3apayxc2f2bq5a6mvryzr"))

(define rust-libpulse-binding-2.28.1
  (crate-source "libpulse-binding" "2.28.1"
                "1zza12f22wf1qs6h71lq1i73aj3kmv3036hqc7qci063vyi5fdgd"))

(define rust-libpulse-sys-1.21.0
  ;; TODO: Check bundled sources.
  (crate-source "libpulse-sys" "1.21.0"
                "16vs0qk6xadckb5qxlrhg0f4jn2zakfd7xih1lk1fb7lzc8f26dw"))

(define rust-libredox-0.1.10
  (crate-source "libredox" "0.1.10"
                "1jswil4ai90s4rh91fg8580x8nikni1zl3wnch4h01nvidqpwvs1"))

(define rust-libredox-0.1.3
  (crate-source "libredox" "0.1.3"
                "139602gzgs0k91zb7dvgj1qh4ynb8g1lbxsswdim18hcb6ykgzy0"))

(define rust-libsqlite3-sys-0.30.1
  ;; TODO: Check bundled sources.
  (crate-source "libsqlite3-sys" "0.30.1"
                "0jcikvgbj84xc7ikdmpc8m4y5lyqgrb9aqblphwk67kv95xgp69f"))

(define rust-libz-sys-1.1.21
  ;; TODO: Check bundled sources.
  (crate-source "libz-sys" "1.1.21"
                "1ajfpf413j9m7kmf4fwvvgv5jxxm5s438f2pfbv2c2vf1vjni6yz"))

(define rust-libz-sys-1.1.22
  ;; TODO: Check bundled sources.
  (crate-source "libz-sys" "1.1.22"
                "07b5wxh0ska996kc0g2hanjhmb4di7ksm6ndljhr4pi0vykyfw4b"))

(define rust-linked-hash-map-0.5.6
  (crate-source "linked-hash-map" "0.5.6"
                "03vpgw7x507g524nx5i1jf5dl8k3kv0fzg8v3ip6qqwbpkqww5q7"))

(define rust-linux-keyutils-0.2.4
  (crate-source "linux-keyutils" "0.2.4"
                "13nipvk2mzk76y7yfsqwnwsqk21x6xy8fkmqz5is99fqbzn4j7kn"))

(define rust-linux-raw-sys-0.11.0
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.11.0"
                "0fghx0nn8nvbz5yzgizfcwd6ap2pislp68j8c1bwyr6sacxkq7fz"))

(define rust-linux-raw-sys-0.4.15
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.4.15"
                "1aq7r2g7786hyxhv40spzf2nhag5xbw2axxc1k8z5k1dsgdm4v6j"))

(define rust-linux-raw-sys-0.4.5
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.4.5"
                "00w52pb2cb4b2880ksyzagmzbyjdmp9ac0w3qfvjv3453fnzvg2p"))

(define rust-litemap-0.7.4
  (crate-source "litemap" "0.7.4"
                "012ili3vppd4952sh6y3qwcd0jkd0bq2qpr9h7cppc8sj11k7saf"))

(define rust-litemap-0.8.0
  (crate-source "litemap" "0.8.0"
                "0mlrlskwwhirxk3wsz9psh6nxcy491n0dh8zl02qgj0jzpssw7i4"))

(define rust-litrs-0.4.2
  (crate-source "litrs" "0.4.2"
                "1v8bxsrkm0w2k9nmbp8hsspy9i1lawajywqdw4hx87rjzqv41rgm"))

(define rust-locale-config-0.3.0
  (crate-source "locale_config" "0.3.0"
                "0d399alr1i7h7yji4vydbdbzd8hp0xaykr7h4rn3yj7l2rdw7lh8"))

(define rust-lock-api-0.4.10
  (crate-source "lock_api" "0.4.10"
                "05nd9nzxqidg24d1k8y5vlc8lz9gscpskrikycib46qbl8brgk61"))

(define rust-lock-api-0.4.11
  (crate-source "lock_api" "0.4.11"
                "0iggx0h4jx63xm35861106af3jkxq06fpqhpkhgw0axi2n38y5iw"))

(define rust-lock-api-0.4.12
  (crate-source "lock_api" "0.4.12"
                "05qvxa6g27yyva25a5ghsg85apdxkvr77yhkyhapj6r8vnf8pbq7"))

(define rust-lock-api-0.4.13
  (crate-source "lock_api" "0.4.13"
                "0rd73p4299mjwl4hhlfj9qr88v3r0kc8s1nszkfmnq2ky43nb4wn"))

(define rust-log-0.4.20
  (crate-source "log" "0.4.20"
                "13rf7wphnwd61vazpxr7fiycin6cb1g8fmvgqg18i464p0y1drmm"))

(define rust-log-0.4.24
  (crate-source "log" "0.4.24"
                "038sdr8l758wi24igjvyx0pwk51jr5rgrmx767p30h10ijja4vix"))

(define rust-log-0.4.28
  (crate-source "log" "0.4.28"
                "0cklpzrpxafbaq1nyxarhnmcw9z3xcjrad3ch55mmr58xw2ha21l"))

(define rust-loop9-0.1.5
  (crate-source "loop9" "0.1.5"
                "0qphc1c0cbbx43pwm6isnwzwbg6nsxjh7jah04n1sg5h4p0qgbhg"))

(define rust-lru-0.16.1
  (crate-source "lru" "0.16.1"
                "1j3ffpdz734s75ish396165gv4yx1zihp6isif9vpyj6kwc4ksdz"))

(define rust-lru-cache-0.1.2
  (crate-source "lru-cache" "0.1.2"
                "071viv6g2p3akwqmfb3c8vsycs5n7kr17b70l7la071jv0d4zqii"))

(define rust-macaddr-1.0.1
  (crate-source "macaddr" "1.0.1"
                "1n5jxn79krlql810c4w3hdkvyqc01141dc5y6fr9sxff2yy0pvms"))

(define rust-mach2-0.4.3
  (crate-source "mach2" "0.4.3"
                "0i6vcnbq5v51whgyidzhf7cbxqrmj2nkw8z0m2ib02rc60mjhh6n"))

(define rust-mail-builder-0.3.2
  (crate-source "mail-builder" "0.3.2"
                "1jzg9y92xbdj2glkpbakhrgv0scd1ih9a2vmxvr81vbha8fqgx95"))

(define rust-mail-parser-0.9.4
  (crate-source "mail-parser" "0.9.4"
                "1z7r9b4fn852s3kqi2mzlg01isfn6wxw9frh6dbsyzxiv3jvkhwk"))

(define rust-mail-send-0.4.9
  (crate-source "mail-send" "0.4.9"
                "0m35h72izqs5gga8axqqqs8yjira4hlkniqbg7jniv80rwjmsmvs"))

(define rust-maildirs-0.2.2
  (crate-source "maildirs" "0.2.2"
                "1xmf16bv2gsk3hwd6nydsi98lhrbwjifv8m5f6j1cv08dydxz58c"))

(define rust-malloc-buf-0.0.6
  (crate-source "malloc_buf" "0.0.6"
                "1jqr77j89pwszv51fmnknzvd53i1nkmcr8rjrvcxhm4dx1zr1fv2"))

(define rust-match-cfg-0.1.0
  (crate-source "match_cfg" "0.1.0"
                "1r5j3zqc3qr8ybcx95bk8q57mkizmgmffj5lmicd4i8d9riyigpz"))

(define rust-matchers-0.1.0
  (crate-source "matchers" "0.1.0"
                "0n2mbk7lg2vf962c8xwzdq96yrc9i0p8dbmm4wa1nnkcp1dhfqw2"))

(define rust-maybe-rayon-0.1.1
  (crate-source "maybe-rayon" "0.1.1"
                "06cmvhj4n36459g327ng5dnj8d58qs472pv5ahlhm7ynxl6g78cf"))

(define rust-md-5-0.10.6
  (crate-source "md-5" "0.10.6"
                "1kvq5rnpm4fzwmyv5nmnxygdhhb2369888a06gdc9pxyrzh7x7nq"))

(define rust-md5-0.7.0
  (crate-source "md5" "0.7.0"
                "0wcps37hrhz59fkhf8di1ppdnqld6l1w5sdy7jp7p51z0i4c8329"))

(define rust-memchr-2.6.2
  (crate-source "memchr" "2.6.2"
                "0vnqrsfm5260gcxyb83ipfd68d51l3azpm81i8dyc6320b8ax1jl"))

(define rust-memchr-2.7.1
  (crate-source "memchr" "2.7.1"
                "0jf1kicqa4vs9lyzj4v4y1p90q0dh87hvhsdd5xvhnp527sw8gaj"))

(define rust-memchr-2.7.4
  (crate-source "memchr" "2.7.4"
                "18z32bhxrax0fnjikv475z7ii718hq457qwmaryixfxsl2qrmjkq"))

(define rust-memchr-2.7.6
  (crate-source "memchr" "2.7.6"
                "0wy29kf6pb4fbhfksjbs05jy2f32r2f3r1ga6qkmpz31k79h0azm"))

(define rust-memmap2-0.9.8
  (crate-source "memmap2" "0.9.8"
                "1dqxjs89krh8cin0k7ksqc9myw9yni9kn8d8cllwq4fn1isrhfl4"))

(define rust-memoffset-0.7.1
  (crate-source "memoffset" "0.7.1"
                "1x2zv8hv9c9bvgmhsjvr9bymqwyxvgbca12cm8xkhpyy5k1r7s2x"))

(define rust-memoffset-0.9.0
  (crate-source "memoffset" "0.9.0"
                "0v20ihhdzkfw1jx00a7zjpk2dcp5qjq6lz302nyqamd9c4f4nqss"))

(define rust-memoffset-0.9.1
  (crate-source "memoffset" "0.9.1"
                "12i17wh9a9plx869g7j4whf62xw68k5zd4k0k5nh6ys5mszid028"))

(define rust-mime-0.3.17
  (crate-source "mime" "0.3.17"
                "16hkibgvb9klh0w0jk5crr5xv90l3wlf77ggymzjmvl1818vnxv8"))

(define rust-mime-guess-2.0.5
  (crate-source "mime_guess" "2.0.5"
                "03jmg3yx6j39mg0kayf7w4a886dl3j15y8zs119zw01ccy74zi7p"))

(define rust-minimad-0.13.1
  (crate-source "minimad" "0.13.1"
                "1lj5szpri8hjf38qrmg0i7vabp9b1rwakm5nly86a63d484dgid9"))

(define rust-minimal-lexical-0.2.1
  (crate-source "minimal-lexical" "0.2.1"
                "16ppc5g84aijpri4jzv14rvcnslvlpphbszc7zzp6vfkddf4qdb8"))

(define rust-miniz-oxide-0.7.1
  (crate-source "miniz_oxide" "0.7.1"
                "1ivl3rbbdm53bzscrd01g60l46lz5krl270487d8lhjvwl5hx0g7"))

(define rust-miniz-oxide-0.7.2
  (crate-source "miniz_oxide" "0.7.2"
                "19qlxb21s6kabgqq61mk7kd1qk2invyygj076jz6i1gj2lz1z0cx"))

(define rust-miniz-oxide-0.7.4
  (crate-source "miniz_oxide" "0.7.4"
                "024wv14aa75cvik7005s5y2nfc8zfidddbd7g55g7sjgnzfl18mq"))

(define rust-miniz-oxide-0.8.2
  (crate-source "miniz_oxide" "0.8.2"
                "1543asrvhla92sby4z6m9ilkg2cmmq8ja6bj84k1vp6f48qfiysg"))

(define rust-miniz-oxide-0.8.9
  (crate-source "miniz_oxide" "0.8.9"
                "05k3pdg8bjjzayq3rf0qhpirq9k37pxnasfn4arbs17phqn6m9qz"))

(define rust-mio-0.8.10
  (crate-source "mio" "0.8.10"
                "02gyaxvaia9zzi4drrw59k9s0j6pa5d1y2kv7iplwjipdqlhngcg"))

(define rust-mio-0.8.11
  (crate-source "mio" "0.8.11"
                "034byyl0ardml5yliy1hmvx8arkmn9rv479pid794sm07ia519m4"))

(define rust-mio-1.0.2
  (crate-source "mio" "1.0.2"
                "1v1cnnn44awxbcfm4zlavwgkvbyg7gp5zzjm8mqf1apkrwflvq40"))

(define rust-mio-1.0.3
  (crate-source "mio" "1.0.3"
                "1gah0h4ia3avxbwym0b6bi6lr6rpysmj9zvw6zis5yq0z0xq91i8"))

(define rust-mio-1.0.4
  (crate-source "mio" "1.0.4"
                "073n3kam3nz8j8had35fd2nn7j6a33pi3y5w3kq608cari2d9gkq"))

(define rust-mml-lib-1.1.1
  (crate-source "mml-lib" "1.1.1"
                "14656qwl8bhp927209g4kspzqja3vyk3kzam81rlfjx3j02cprrr"))

(define rust-moka-0.12.8
  (crate-source "moka" "0.12.8"
                "0vrbsd86bdnliwgnzwqw6gi3x7n4fl8gnck4wzfx4xfr9pmn5krj"))

(define rust-moxcms-0.7.5
  (crate-source "moxcms" "0.7.5"
                "026df3qpxn430dlngpj3gjip0m9280g3asvbia5dpsjsjfl2zlyx"))

(define rust-mutate-once-0.1.2
  (crate-source "mutate_once" "0.1.2"
                "1byj4yz6h0mdx3mmj0mq23ma59kw41pckspr2gz8rl22k0y27lhk"))

(define rust-nanohtml2text-0.1.5
  (crate-source "nanohtml2text" "0.1.5"
                "0ryqldqv3668k0658nyp9rngbb59xbqxhfrlh2i2skbm8lvdqjwx"))

(define rust-new-debug-unreachable-1.0.6
  (crate-source "new_debug_unreachable" "1.0.6"
                "11phpf1mjxq6khk91yzcbd3ympm78m3ivl7xg6lg2c0lf66fy3k5"))

(define rust-newline-converter-0.3.0
  (crate-source "newline-converter" "0.3.0"
                "0zyw2hyjl89rj1zmp9n8fq69pbfp9zl1cbal73agxjxixjbv1dj7"))

(define rust-nix-0.26.4
  (crate-source "nix" "0.26.4"
                "06xgl4ybb8pvjrbmc3xggbgk3kbs1j0c4c0nzdfrmpbgrkrym2sr"))

(define rust-nix-0.27.1
  (crate-source "nix" "0.27.1"
                "0ly0kkmij5f0sqz35lx9czlbk6zpihb7yh1bsy4irzwfd2f4xc1f"))

(define rust-nix-0.29.0
  (crate-source "nix" "0.29.0"
                "0ikvn7s9r2lrfdm3mx1h7nbfjvcc6s9vxdzw7j5xfkd2qdnp9qki"))

(define rust-nom-7.1.3
  (crate-source "nom" "7.1.3"
                "0jha9901wxam390jcf5pfa0qqfrgh8li787jx2ip0yk5b8y9hwyj"))

(define rust-noop-proc-macro-0.3.0
  (crate-source "noop_proc_macro" "0.3.0"
                "1j2v1c6ric4w9v12h34jghzmngcwmn0hll1ywly4h6lcm4rbnxh6"))

(define rust-normpath-1.5.0
  (crate-source "normpath" "1.5.0"
                "16z68q809749ky2vl72f3lqnhf3vjclvcc3y2z5v8m2nj0msn8xz"))

(define rust-notify-6.1.1
  (crate-source "notify" "6.1.1"
                "0bad98r0ilkhhq2jg3zs11zcqasgbvxia8224wpasm74n65vs1b2"))

(define rust-notify-8.2.0
  (crate-source "notify" "8.2.0"
                "1hrb83451vm5cpjw83nz5skgwjg5ara28zq8nxsqbzsif690fgad"))

(define rust-notify-types-2.0.0
  (crate-source "notify-types" "2.0.0"
                "0pcjm3wnvb7pvzw6mn89csv64ip0xhx857kr8jic5vddi6ljc22y"))

(define rust-notmuch-0.8.0
  (crate-source "notmuch" "0.8.0"
                "0i6xc7lv10m2sq6vlpjr5wxmlxihvd0v4f5if75r2kwz8ji12pg2"))

(define rust-nu-ansi-term-0.46.0
  (crate-source "nu-ansi-term" "0.46.0"
                "115sywxh53p190lyw97alm14nc004qj5jm5lvdj608z84rbida3p"))

(define rust-num-0.4.3
  (crate-source "num" "0.4.3"
                "08yb2fc1psig7pkzaplm495yp7c30m4pykpkwmi5bxrgid705g9m"))

(define rust-num-bigint-0.4.6
  (crate-source "num-bigint" "0.4.6"
                "1f903zd33i6hkjpsgwhqwi2wffnvkxbn6rv4mkgcjcqi7xr4zr55"))

(define rust-num-bigint-dig-0.8.4
  (crate-source "num-bigint-dig" "0.8.4"
                "0lb12df24wgxxbspz4gw1sf1kdqwvpdcpwq4fdlwg4gj41c1k16w"))

(define rust-num-complex-0.4.6
  (crate-source "num-complex" "0.4.6"
                "15cla16mnw12xzf5g041nxbjjm9m85hdgadd5dl5d0b30w9qmy3k"))

(define rust-num-conv-0.1.0
  (crate-source "num-conv" "0.1.0"
                "1ndiyg82q73783jq18isi71a7mjh56wxrk52rlvyx0mi5z9ibmai"))

(define rust-num-cpus-1.16.0
  (crate-source "num_cpus" "1.16.0"
                "0hra6ihpnh06dvfvz9ipscys0xfqa9ca9hzp384d5m02ssvgqqa1"))

(define rust-num-derive-0.3.3
  (crate-source "num-derive" "0.3.3"
                "0gbl94ckzqjdzy4j8b1p55mz01g6n1l9bckllqvaj0wfz7zm6sl7"))

(define rust-num-derive-0.4.2
  (crate-source "num-derive" "0.4.2"
                "00p2am9ma8jgd2v6xpsz621wc7wbn1yqi71g15gc3h67m7qmafgd"))

(define rust-num-integer-0.1.46
  (crate-source "num-integer" "0.1.46"
                "13w5g54a9184cqlbsq80rnxw4jj4s0d8wv75jsq5r2lms8gncsbr"))

(define rust-num-iter-0.1.45
  (crate-source "num-iter" "0.1.45"
                "1gzm7vc5g9qsjjl3bqk9rz1h6raxhygbrcpbfl04swlh0i506a8l"))

(define rust-num-rational-0.4.2
  (crate-source "num-rational" "0.4.2"
                "093qndy02817vpgcqjnj139im3jl7vkq4h68kykdqqh577d18ggq"))

(define rust-num-traits-0.2.18
  (crate-source "num-traits" "0.2.18"
                "0yjib8p2p9kzmaz48xwhs69w5dh1wipph9jgnillzd2x33jz03fs"))

(define rust-num-traits-0.2.19
  (crate-source "num-traits" "0.2.19"
                "0h984rhdkkqd4ny9cif7y2azl3xdfb7768hb9irhpsch4q3gq787"))

(define rust-oauth-lib-2.0.0
  (crate-source "oauth-lib" "2.0.0"
                "0apraw0b90j8cb0x54a6k3wv3j6nqx2y1np3f4crspmwbxzkcjjx"))

(define rust-oauth2-5.0.0-rc.1
  (crate-source "oauth2" "5.0.0-rc.1"
                "1gw5cxhkkf65skz1ggkhfpgh8rn3f7nvww1gdl1jjbb07kd8blr3"))

(define rust-objc-0.2.7
  (crate-source "objc" "0.2.7"
                "1cbpf6kz8a244nn1qzl3xyhmp05gsg4n313c9m3567625d3innwi"))

(define rust-objc-foundation-0.1.1
  (crate-source "objc-foundation" "0.1.1"
                "1y9bwb3m5fdq7w7i4bnds067dhm4qxv4m1mbg9y61j9nkrjipp8s"))

(define rust-objc-id-0.1.1
  (crate-source "objc_id" "0.1.1"
                "0fq71hnp2sdblaighjc82yrac3adfmqzhpr11irhvdfp9gdlsbf9"))

(define rust-objc-sys-0.3.5
  ;; TODO: Check bundled sources.
  (crate-source "objc-sys" "0.3.5"
                "0423gry7s3rmz8s3pzzm1zy5mdjif75g6dbzc2lf2z0c77fipffd"))

(define rust-objc2-0.5.2
  (crate-source "objc2" "0.5.2"
                "015qa2d3vh7c1j2736h5wjrznri7x5ic35vl916c22gzxva8b9s6"))

(define rust-objc2-0.6.2
  (crate-source "objc2" "0.6.2"
                "1g3qa1vxp6nlh4wllll921z299d3s1is31m1ccasd8pklxxka7sn"))

(define rust-objc2-app-kit-0.2.2
  (crate-source "objc2-app-kit" "0.2.2"
                "1zqyi5l1bm26j1bgmac9783ah36m5kcrxlqp5carglnpwgcrms74"))

(define rust-objc2-core-data-0.2.2
  (crate-source "objc2-core-data" "0.2.2"
                "1vvk8zjylfjjj04dzawydmqqz5ajvdkhf22cnb07ihbiw14vyzv1"))

(define rust-objc2-core-image-0.2.2
  (crate-source "objc2-core-image" "0.2.2"
                "102csfb82zi2sbzliwsfd589ckz0gysf7y6434c9zj97lmihj9jm"))

(define rust-objc2-encode-4.1.0
  (crate-source "objc2-encode" "4.1.0"
                "0cqckp4cpf68mxyc2zgnazj8klv0z395nsgbafa61cjgsyyan9gg"))

(define rust-objc2-foundation-0.2.2
  (crate-source "objc2-foundation" "0.2.2"
                "1a6mi77jsig7950vmx9ydvsxaighzdiglk5d229k569pvajkirhf"))

(define rust-objc2-foundation-0.3.1
  (crate-source "objc2-foundation" "0.3.1"
                "0g5hl47dxzabs7wndcg6kz3q137v9hwfay1jd2da1q9gglj3224h"))

(define rust-objc2-metal-0.2.2
  (crate-source "objc2-metal" "0.2.2"
                "1mmdga66qpxrcfq3gxxhysfx3zg1hpx4z886liv3j0pnfq9bl36x"))

(define rust-objc2-quartz-core-0.2.2
  (crate-source "objc2-quartz-core" "0.2.2"
                "0ynw8819c36l11rim8n0yzk0fskbzrgaqayscyqi8swhzxxywaz4"))

(define rust-object-0.32.0
  (crate-source "object" "0.32.0"
                "1ghynapcbgzrmnbwmmxj129dbzvmh0hwx8bplmh8ra5f0yympb3p"))

(define rust-object-0.32.2
  (crate-source "object" "0.32.2"
                "0hc4cjwyngiy6k51hlzrlsxgv5z25vv7c2cp0ky1lckfic0259m6"))

(define rust-octseq-0.5.2
  (crate-source "octseq" "0.5.2"
                "04pycbrcxlmhxqmrs4jgd0kqjk9pwjil6zr4fp2wwi4wgjikqv0j"))

(define rust-once-cell-1.18.0
  (crate-source "once_cell" "1.18.0"
                "0vapcd5ambwck95wyz3ymlim35jirgnqn9a0qmi19msymv95v2yx"))

(define rust-once-cell-1.19.0
  (crate-source "once_cell" "1.19.0"
                "14kvw7px5z96dk4dwdm1r9cqhhy2cyj1l5n5b29mynbb8yr15nrz"))

(define rust-once-cell-1.20.2
  (crate-source "once_cell" "1.20.2"
                "0xb7rw1aqr7pa4z3b00y7786gyf8awx2gca3md73afy76dzgwq8j"))

(define rust-once-cell-1.21.3
  (crate-source "once_cell" "1.21.3"
                "0b9x77lb9f1j6nqgf5aka4s2qj0nly176bpbrv6f9iakk5ff3xa2"))

(define rust-once-cell-polyfill-1.70.1
  (crate-source "once_cell_polyfill" "1.70.1"
                "1bg0w99srq8h4mkl68l1mza2n2f2hvrg0n8vfa3izjr5nism32d4"))

(define rust-open-1.7.1
  (crate-source "open" "1.7.1"
                "00828zcxdy3r38inz48jgnszgvqgi1a3bi2rrhij86mqsqq7msnw"))

(define rust-open-5.3.2
  (crate-source "open" "5.3.2"
                "15ggfx1p8rl7w4rr1n5qj1wxy1kk7757lsjpyc947a9fwri3aj72"))

(define rust-opener-0.8.3
  (crate-source "opener" "0.8.3"
                "0isfar4r3h25kf1z35mz8r1sdh8gilm3a51akp4007mr5ab2946b"))

(define rust-openssl-probe-0.1.5
  (crate-source "openssl-probe" "0.1.5"
                "1kq18qm48rvkwgcggfkqq6pm948190czqc94d6bm2sir5hq1l0gz"))

(define rust-option-ext-0.2.0
  (crate-source "option-ext" "0.2.0"
                "0zbf7cx8ib99frnlanpyikm1bx8qn8x602sw1n7bg6p9x94lyx04"))

(define rust-ordered-stream-0.2.0
  (crate-source "ordered-stream" "0.2.0"
                "0l0xxp697q7wiix1gnfn66xsss7fdhfivl2k7bvpjs4i3lgb18ls"))

(define rust-os-str-bytes-6.6.1
  (crate-source "os_str_bytes" "6.6.1"
                "1885z1x4sm86v5p41ggrl49m58rbzzhd1kj72x46yy53p62msdg2"))

(define rust-ouroboros-0.15.6
  (crate-source "ouroboros" "0.15.6"
                "1nvjra9dana2g6kxv3397qrgpyw6lknzya6lzs1s1llbap8qndg1"))

(define rust-ouroboros-macro-0.15.6
  (crate-source "ouroboros_macro" "0.15.6"
                "1dsn37vds4qpkzscmwaw17dv3m5m7a7j9qby8dsac19ks3622zaz"))

(define rust-overload-0.1.1
  (crate-source "overload" "0.1.1"
                "0fdgbaqwknillagy1xq7xfgv60qdbk010diwl7s1p0qx7hb16n5i"))

(define rust-owo-colors-3.5.0
  (crate-source "owo-colors" "3.5.0"
                "0vyvry6ba1xmpd45hpi6savd8mbx09jpmvnnwkf6z62pk6s4zc61"))

(define rust-p256-0.13.2
  (crate-source "p256" "0.13.2"
                "0jyd3c3k239ybs59ixpnl7dqkmm072fr1js8kh7ldx58bzc3m1n9"))

(define rust-p384-0.13.0
  (crate-source "p384" "0.13.0"
                "02cjlxdvxwvhmnckqnydqpvrwhf5raj67q300d66m7y6pi8nyy3h"))

(define rust-pango-0.18.3
  (crate-source "pango" "0.18.3"
                "1r5ygq7036sv7w32kp8yxr6vgggd54iaavh3yckanmq4xg0px8kw"))

(define rust-pango-sys-0.18.0
  ;; TODO: Check bundled sources.
  (crate-source "pango-sys" "0.18.0"
                "1iaxalcaaj59cl9n10svh4g50v8jrc1a36kd7n9yahx8j7ikfrs3"))

(define rust-parking-2.2.1
  (crate-source "parking" "2.2.1"
                "1fnfgmzkfpjd69v4j9x737b1k8pnn054bvzcn5dm3pkgq595d3gk"))

(define rust-parking-lot-0.11.2
  (crate-source "parking_lot" "0.11.2"
                "16gzf41bxmm10x82bla8d6wfppy9ym3fxsmdjyvn61m66s0bf5vx"))

(define rust-parking-lot-0.12.1
  (crate-source "parking_lot" "0.12.1"
                "13r2xk7mnxfc5g0g6dkdxqdqad99j7s7z8zhzz4npw5r0g0v4hip"))

(define rust-parking-lot-0.12.3
  (crate-source "parking_lot" "0.12.3"
                "09ws9g6245iiq8z975h8ycf818a66q3c6zv4b5h8skpm7hc1igzi"))

(define rust-parking-lot-0.12.4
  (crate-source "parking_lot" "0.12.4"
                "04sab1c7304jg8k0d5b2pxbj1fvgzcf69l3n2mfpkdb96vs8pmbh"))

(define rust-parking-lot-core-0.8.6
  (crate-source "parking_lot_core" "0.8.6"
                "1p2nfcbr0b9lm9rglgm28k6mwyjwgm4knipsmqbgqaxdy3kcz8k0"))

(define rust-parking-lot-core-0.9.10
  (crate-source "parking_lot_core" "0.9.10"
                "1y3cf9ld9ijf7i4igwzffcn0xl16dxyn4c5bwgjck1dkgabiyh0y"))

(define rust-parking-lot-core-0.9.11
  (crate-source "parking_lot_core" "0.9.11"
                "19g4d6m5k4ggacinqprnn8xvdaszc3y5smsmbz1adcdmaqm8v0xw"))

(define rust-parking-lot-core-0.9.8
  (crate-source "parking_lot_core" "0.9.8"
                "0ixlak319bpzldq20yvyfqk0y1vi736zxbw101jvzjp7by30rw4k"))

(define rust-parking-lot-core-0.9.9
  (crate-source "parking_lot_core" "0.9.9"
                "13h0imw1aq86wj28gxkblhkzx6z1gk8q18n0v76qmmj6cliajhjc"))

(define rust-paste-1.0.15
  (crate-source "paste" "1.0.15"
                "02pxffpdqkapy292harq6asfjvadgp1s005fip9ljfsn9fvxgh2p"))

(define rust-pathdiff-0.2.3
  (crate-source "pathdiff" "0.2.3"
                "1lrqp4ip05df8dzldq6gb2c1sq2gs54gly8lcnv3rhav1qhwx56z"))

(define rust-pem-rfc7468-0.7.0
  (crate-source "pem-rfc7468" "0.7.0"
                "04l4852scl4zdva31c1z6jafbak0ni5pi0j38ml108zwzjdrrcw8"))

(define rust-percent-encoding-2.3.1
  (crate-source "percent-encoding" "2.3.1"
                "0gi8wgx0dcy8rnv1kywdv98lwcx67hz0a0zwpib5v2i08r88y573"))

(define rust-percent-encoding-2.3.2
  (crate-source "percent-encoding" "2.3.2"
                "083jv1ai930azvawz2khv7w73xh8mnylk7i578cifndjn5y64kwv"))

(define rust-petgraph-0.6.5
  (crate-source "petgraph" "0.6.5"
                "1ns7mbxidnn2pqahbbjccxkrqkrll2i5rbxx43ns6rh6fn3cridl"))

(define rust-pgp-0.10.2
  (crate-source "pgp" "0.10.2"
                "0cp1pdiv0v1s2llk30xk8zc9r84hpsndlggycdbviadzhphgiq97"))

(define rust-pgp-lib-1.0.0
  (crate-source "pgp-lib" "1.0.0"
                "1mzywmwc73xjxsbqqj2vpqjsmz563f3pg78hswqsmnj3p16adcb2"))

(define rust-phf-0.11.2
  (crate-source "phf" "0.11.2"
                "1p03rsw66l7naqhpgr1a34r9yzi1gv9jh16g3fsk6wrwyfwdiqmd"))

(define rust-phf-0.13.1
  (crate-source "phf" "0.13.1"
                "1pzswx5gdglgjgp4azyzwyr4gh031r0kcnpqq6jblga72z3jsmn1"))

(define rust-phf-generator-0.11.2
  (crate-source "phf_generator" "0.11.2"
                "1c14pjyxbcpwkdgw109f7581cc5fa3fnkzdq1ikvx7mdq9jcrr28"))

(define rust-phf-generator-0.13.1
  (crate-source "phf_generator" "0.13.1"
                "0dwpp11l41dy9mag4phkyyvhpf66lwbp79q3ik44wmhyfqxcwnhk"))

(define rust-phf-macros-0.11.2
  (crate-source "phf_macros" "0.11.2"
                "0js61lc0bhzzrbd9vhpcqp11vvwckdkz3g7k95z5h1k651p68i1l"))

(define rust-phf-macros-0.13.1
  (crate-source "phf_macros" "0.13.1"
                "1vv9h8pr7xh18sigpvq1hxc8q9nmjmv6gdpqsp65krxiahmh6bw1"))

(define rust-phf-shared-0.11.2
  (crate-source "phf_shared" "0.11.2"
                "0azphb0a330ypqx3qvyffal5saqnks0xvl8rj73jlk3qxxgbkz4h"))

(define rust-phf-shared-0.13.1
  (crate-source "phf_shared" "0.13.1"
                "0rpjchnswm0x5l4mz9xqfpw0j4w68sjvyqrdrv13h7lqqmmyyzz5"))

(define rust-pico-args-0.5.0
  (crate-source "pico-args" "0.5.0"
                "05d30pvxd6zlnkg2i3ilr5a70v3f3z2in18m67z25vinmykngqav"))

(define rust-pimalaya-tui-0.2.2
  (crate-source "pimalaya-tui" "0.2.2"
                "072lkdnhsw04i9vbfrmry7mr6hhxpm9hh9rw4kykvk3azp6vhxhc"))

(define rust-pin-project-1.1.4
  (crate-source "pin-project" "1.1.4"
                "1q07737j774zxffdrypncdsrc5zx7dffw6l4dzanni9c8jhc80h3"))

(define rust-pin-project-1.1.5
  (crate-source "pin-project" "1.1.5"
                "1cxl146x0q7lawp0m1826wsgj8mmmfs6ja8q7m6f7ff5j6vl7gxn"))

(define rust-pin-project-internal-1.1.4
  (crate-source "pin-project-internal" "1.1.4"
                "141nmib9lqbisrf8rz6fayy6l4fiw2r547h6af6npiy9c0mh8v16"))

(define rust-pin-project-internal-1.1.5
  (crate-source "pin-project-internal" "1.1.5"
                "0r9r4ivwiyqf45sv6b30l1dx282lxaax2f6gl84jwa3q590s8f1g"))

(define rust-pin-project-lite-0.2.13
  (crate-source "pin-project-lite" "0.2.13"
                "0n0bwr5qxlf0mhn2xkl36sy55118s9qmvx2yl5f3ixkb007lbywa"))

(define rust-pin-project-lite-0.2.16
  (crate-source "pin-project-lite" "0.2.16"
                "16wzc7z7dfkf9bmjin22f5282783f6mdksnr0nv0j5ym5f9gyg1v"))

(define rust-pin-utils-0.1.0
  (crate-source "pin-utils" "0.1.0"
                "117ir7vslsl2z1a7qzhws4pd01cg2d3338c47swjyvqv2n60v1wb"))

(define rust-piper-0.2.4
  (crate-source "piper" "0.2.4"
                "0rn0mjjm0cwagdkay77wgmz3sqf8fqmv9d9czm79mvr2yj8c9j4n"))

(define rust-pkcs1-0.7.5
  (crate-source "pkcs1" "0.7.5"
                "0zz4mil3nchnxljdfs2k5ab1cjqn7kq5lqp62n9qfix01zqvkzy8"))

(define rust-pkcs8-0.10.2
  (crate-source "pkcs8" "0.10.2"
                "1dx7w21gvn07azszgqd3ryjhyphsrjrmq5mmz1fbxkj5g0vv4l7r"))

(define rust-pkg-config-0.3.29
  (crate-source "pkg-config" "0.3.29"
                "1jy6158v1316khkpmq2sjj1vgbnbnw51wffx7p0k0l9h9vlys019"))

(define rust-pkg-config-0.3.31
  (crate-source "pkg-config" "0.3.31"
                "1wk6yp2phl91795ia0lwkr3wl4a9xkrympvhqq8cxk4d75hwhglm"))

(define rust-pkg-config-0.3.32
  (crate-source "pkg-config" "0.3.32"
                "0k4h3gnzs94sjb2ix6jyksacs52cf1fanpwsmlhjnwrdnp8dppby"))

(define rust-plist-1.8.0
  (crate-source "plist" "1.8.0"
                "01qyv51ljbvhjbg8mva5c802b3dzrr95y6nd23wjh52xbjhvw3kl"))

(define rust-png-0.17.16
  (crate-source "png" "0.17.16"
                "09kmkms9fmkbkarw0lnf0scqvjwwg3r7riddag0i3q39r0pil5c2"))

(define rust-png-0.18.0
  (crate-source "png" "0.18.0"
                "187jf0m873qn5biix8z7gjdsyf8r6vj3yr495pa0jja6i39wxflp"))

(define rust-polling-3.7.4
  (crate-source "polling" "3.7.4"
                "0bs4nhwfwsvlzlhah2gbhj3aa9ynvchv2g350wapswh26a65c156"))

(define rust-potential-utf-0.1.3
  (crate-source "potential_utf" "0.1.3"
                "12mhwvhpvvim6xqp6ifgkh1sniv9j2cmid6axn10fnjvpsnikpw4"))

(define rust-powerfmt-0.2.0
  (crate-source "powerfmt" "0.2.0"
                "14ckj2xdpkhv3h6l5sdmb9f1d57z8hbfpdldjc2vl5givq2y77j3"))

(define rust-ppv-lite86-0.2.17
  (crate-source "ppv-lite86" "0.2.17"
                "1pp6g52aw970adv3x2310n7glqnji96z0a9wiamzw89ibf0ayh2v"))

(define rust-ppv-lite86-0.2.20
  (crate-source "ppv-lite86" "0.2.20"
                "017ax9ssdnpww7nrl1hvqh2lzncpv04nnsibmnw9nxjnaqlpp5bp"))

(define rust-ppv-lite86-0.2.21
  (crate-source "ppv-lite86" "0.2.21"
                "1abxx6qz5qnd43br1dd9b2savpihzjza8gb4fbzdql1gxp2f7sl5"))

(define rust-primeorder-0.13.6
  (crate-source "primeorder" "0.13.6"
                "1rp16710mxksagcjnxqjjq9r9wf5vf72fs8wxffnvhb6i6hiqgim"))

(define rust-proc-macro-crate-1.3.1
  (crate-source "proc-macro-crate" "1.3.1"
                "069r1k56bvgk0f58dm5swlssfcp79im230affwk6d9ck20g04k3z"))

(define rust-proc-macro-crate-2.0.2
  (crate-source "proc-macro-crate" "2.0.2"
                "092x5acqnic14cw6vacqap5kgknq3jn4c6jij9zi6j85839jc3xh"))

(define rust-proc-macro-crate-3.2.0
  (crate-source "proc-macro-crate" "3.2.0"
                "0yzsqnavb3lmrcsmbrdjfrky9vcbl46v59xi9avn0796rb3likwf"))

(define rust-proc-macro-error-1.0.4
  (crate-source "proc-macro-error" "1.0.4"
                "1373bhxaf0pagd8zkyd03kkx6bchzf6g0dkwrwzsnal9z47lj9fs"))

(define rust-proc-macro-error-attr-1.0.4
  (crate-source "proc-macro-error-attr" "1.0.4"
                "0sgq6m5jfmasmwwy8x4mjygx5l7kp8s4j60bv25ckv2j1qc41gm1"))

(define rust-proc-macro2-1.0.101
  (crate-source "proc-macro2" "1.0.101"
                "1pijhychkpl7rcyf1h7mfk6gjfii1ywf5n0snmnqs5g4hvyl7bl9"))

(define rust-proc-macro2-1.0.78
  (crate-source "proc-macro2" "1.0.78"
                "1bjak27pqdn4f4ih1c9nr3manzyavsgqmf76ygw9k76q8pb2lhp2"))

(define rust-proc-macro2-1.0.86
  (crate-source "proc-macro2" "1.0.86"
                "0xrv22p8lqlfdf1w0pj4si8n2ws4aw0kilmziwf0vpv5ys6rwway"))

(define rust-proc-macro2-1.0.93
  (crate-source "proc-macro2" "1.0.93"
                "169dw9wch753if1mgyi2nfl1il77gslvh6y2q46qplprwml6m530"))

(define rust-proc-status-0.1.1
  (crate-source "proc-status" "0.1.1"
                "04lp8kdj75m8s1hwxslyzz3fdgbs6zy4zfjhg2s7cysyj6nc1q7h"))

(define rust-process-lib-1.0.0
  (crate-source "process-lib" "1.0.0"
                "0ilcj6bzg0ncg5hi8jmc5l7p9j76vmb6zpmvxhnb7xpa7b9pxd1y"))

(define rust-profiling-1.0.17
  (crate-source "profiling" "1.0.17"
                "0wqp6i1bl7azy9270dp92srbbr55mgdh9qnk5b1y44lyarmlif1y"))

(define rust-profiling-procmacros-1.0.17
  (crate-source "profiling-procmacros" "1.0.17"
                "0nrxdh5r723raxbs136jmjx46p0c5qgai8jwz4j555mn0ad7ywaj"))

(define rust-pxfm-0.1.24
  (crate-source "pxfm" "0.1.24"
                "1phz82xji3yr5bypdqbwl4x96bs7zfvqk8zlq2jssn92n0wv7yc3"))

(define rust-qoi-0.4.1
  (crate-source "qoi" "0.4.1"
                "00c0wkb112annn2wl72ixyd78mf56p4lxkhlmsggx65l3v3n8vbz"))

(define rust-quanta-0.12.3
  (crate-source "quanta" "0.12.3"
                "19cds3yg3ri0wrypn7b3j2x8qf1w9rkw5yl4nah2i4k1fyj6flcf"))

(define rust-quick-error-1.2.3
  (crate-source "quick-error" "1.2.3"
                "1q6za3v78hsspisc197bg3g7rpc989qycy8ypr8ap8igv10ikl51"))

(define rust-quick-error-2.0.1
  (crate-source "quick-error" "2.0.1"
                "18z6r2rcjvvf8cn92xjhm2qc3jpd1ljvcbf12zv0k9p565gmb4x9"))

(define rust-quick-xml-0.38.3
  (crate-source "quick-xml" "0.38.3"
                "12bvsbnnmlnq9xg9in3h3080ag3sisafgpcn7lqyzhkz93kk58j2"))

(define rust-quote-1.0.35
  (crate-source "quote" "1.0.35"
                "1vv8r2ncaz4pqdr78x7f138ka595sp2ncr1sa2plm4zxbsmwj7i9"))

(define rust-quote-1.0.36
  (crate-source "quote" "1.0.36"
                "19xcmh445bg6simirnnd4fvkmp6v2qiwxh5f6rw4a70h76pnm9qg"))

(define rust-quote-1.0.38
  (crate-source "quote" "1.0.38"
                "1k0s75w61k6ch0rs263r4j69b7vj1wadqgb9dia4ylc9mymcqk8f"))

(define rust-quote-1.0.41
  (crate-source "quote" "1.0.41"
                "1lg108nb57lwbqlnpsii89cchk6i8pkcvrv88xh1p7a9gdz7c9ff"))

(define rust-r-efi-5.3.0
  (crate-source "r-efi" "5.3.0"
                "03sbfm3g7myvzyylff6qaxk4z6fy76yv860yy66jiswc2m6b7kb9"))

(define rust-rand-0.8.5
  (crate-source "rand" "0.8.5"
                "013l6931nn7gkc23jz5mm3qdhf93jjf0fg64nz2lp4i51qd8vbrl"))

(define rust-rand-chacha-0.3.1
  (crate-source "rand_chacha" "0.3.1"
                "123x2adin558xbhvqb8w4f6syjsdkmqff8cxwhmjacpsl1ihmhg6"))

(define rust-rand-core-0.6.4
  (crate-source "rand_core" "0.6.4"
                "0b4j2v4cb5krak1pv6kakv4sz6xcwbrmy2zckc32hsigbrwy82zc"))

(define rust-rav1e-0.7.1
  (crate-source "rav1e" "0.7.1"
                "1sawva6nmj2fvynydbcirr3nb7wjyg0id2hz2771qnv6ly0cx1yd"))

(define rust-ravif-0.11.20
  (crate-source "ravif" "0.11.20"
                "0sryi4qzwv3rjhy4h0z9fb0zw3k3i814j3ck2psvjsnivmpw49aq"))

(define rust-raw-cpuid-11.2.0
  (crate-source "raw-cpuid" "11.2.0"
                "1c77cmsn7rj6knwwrg2y9nl46wss5p9jq3wzxvr1a5k6bhql1chs"))

(define rust-rayon-1.10.0
  (crate-source "rayon" "1.10.0"
                "1ylgnzwgllajalr4v00y4kj22klq2jbwllm70aha232iah0sc65l"))

(define rust-rayon-1.11.0
  (crate-source "rayon" "1.11.0"
                "13x5fxb7rn4j2yw0cr26n7782jkc7rjzmdkg42qxk3xz0p8033rn"))

(define rust-rayon-core-1.12.1
  (crate-source "rayon-core" "1.12.1"
                "1qpwim68ai5h0j7axa8ai8z0payaawv3id0lrgkqmapx7lx8fr8l"))

(define rust-rayon-core-1.13.0
  (crate-source "rayon-core" "1.13.0"
                "14dbr0sq83a6lf1rfjq5xdpk5r6zgzvmzs5j6110vlv2007qpq92"))

(define rust-redox-syscall-0.2.16
  (crate-source "redox_syscall" "0.2.16"
                "16jicm96kjyzm802cxdd1k9jmcph0db1a4lhslcnhjsvhp0mhnpv"))

(define rust-redox-syscall-0.3.5
  (crate-source "redox_syscall" "0.3.5"
                "0acgiy2lc1m2vr8cr33l5s7k9wzby8dybyab1a9p753hcbr68xjn"))

(define rust-redox-syscall-0.4.1
  (crate-source "redox_syscall" "0.4.1"
                "1aiifyz5dnybfvkk4cdab9p2kmphag1yad6iknc7aszlxxldf8j7"))

(define rust-redox-syscall-0.5.17
  (crate-source "redox_syscall" "0.5.17"
                "0xrvpchkaxph3r5ww2i04v9nwg3843fp3prf8kqlh1gv01b4c1sl"))

(define rust-redox-syscall-0.5.8
  (crate-source "redox_syscall" "0.5.8"
                "0d48ylyd6gsamynyp257p6n2zl4dw2fhnn5z9y3nhgpri6rn5a03"))

(define rust-redox-users-0.4.6
  (crate-source "redox_users" "0.4.6"
                "0hya2cxx6hxmjfxzv9n8rjl5igpychav7zfi1f81pz6i4krry05s"))

(define rust-regex-1.10.3
  (crate-source "regex" "1.10.3"
                "05cvihqy0wgnh9i8a9y2n803n5azg2h0b7nlqy6rsvxhy00vwbdn"))

(define rust-regex-1.11.1
  (crate-source "regex" "1.11.1"
                "148i41mzbx8bmq32hsj1q4karkzzx5m60qza6gdw4pdc9qdyyi5m"))

(define rust-regex-1.11.3
  (crate-source "regex" "1.11.3"
                "0b58ya98c4i5cjjiwhpcnjr61cv9g143qhdwhsryggj09098hllb"))

(define rust-regex-automata-0.1.10
  (crate-source "regex-automata" "0.1.10"
                "0ci1hvbzhrfby5fdpf4ganhf7kla58acad9i1ff1p34dzdrhs8vc"))

(define rust-regex-automata-0.3.9
  (crate-source "regex-automata" "0.3.9"
                "1agg6ymbgjydj3q31ay6dbzgp3i5cnrnygpylczqj623xs93xcjr"))

(define rust-regex-automata-0.4.11
  (crate-source "regex-automata" "0.4.11"
                "1bawj908pxixpggcnma3xazw53mwyz68lv9hn4yg63nlhv7bjgl3"))

(define rust-regex-automata-0.4.5
  (crate-source "regex-automata" "0.4.5"
                "1karc80mx15z435rm1jg3sqylnc58nxi15gqypcd1inkzzpqgfav"))

(define rust-regex-automata-0.4.9
  (crate-source "regex-automata" "0.4.9"
                "02092l8zfh3vkmk47yjc8d631zhhcd49ck2zr133prvd3z38v7l0"))

(define rust-regex-syntax-0.6.29
  (crate-source "regex-syntax" "0.6.29"
                "1qgj49vm6y3zn1hi09x91jvgkl2b1fiaq402skj83280ggfwcqpi"))

(define rust-regex-syntax-0.7.5
  (crate-source "regex-syntax" "0.7.5"
                "1nhjmqdlakfi4yb8lh7vbbh71dsy90jjvrjvvnrih6larldgpdfv"))

(define rust-regex-syntax-0.8.2
  (crate-source "regex-syntax" "0.8.2"
                "17rd2s8xbiyf6lb4aj2nfi44zqlj98g2ays8zzj2vfs743k79360"))

(define rust-regex-syntax-0.8.5
  (crate-source "regex-syntax" "0.8.5"
                "0p41p3hj9ww7blnbwbj9h7rwxzxg0c1hvrdycgys8rxyhqqw859b"))

(define rust-regex-syntax-0.8.6
  (crate-source "regex-syntax" "0.8.6"
                "00chjpglclfskmc919fj5aq308ffbrmcn7kzbkz92k231xdsmx6a"))

(define rust-resolv-conf-0.7.0
  (crate-source "resolv-conf" "0.7.0"
                "005sk8r1php2g41yn7fdf1sn8cafyaqm6jxia42h2v88saa47r2j"))

(define rust-resvg-0.45.1
  (crate-source "resvg" "0.45.1"
                "0hyz89wyasn0wdnpw7qy5iyl4xv3yx36hk3crb4h6pm5q2c8g4m8"))

(define rust-rfc6979-0.4.0
  (crate-source "rfc6979" "0.4.0"
                "1chw95jgcfrysyzsq6a10b1j5qb7bagkx8h0wda4lv25in02mpgq"))

(define rust-rgb-0.8.52
  (crate-source "rgb" "0.8.52"
                "1km115a9lblf9pldvx51dmmg30y8ms4ka67hvas2ndcq556qhshc"))

(define rust-ring-0.17.14
  (crate-source "ring" "0.17.14"
                "1dw32gv19ccq4hsx3ribhpdzri1vnrlcfqb2vj41xn4l49n9ws54"))

(define rust-ring-0.17.8
  (crate-source "ring" "0.17.8"
                "03fwlb1ssrmfxdckvqv033pfmk01rhx9ynwi7r186dcfcp5s8zy1"))

(define rust-rip-starttls-0.1.0
  (crate-source "rip-starttls" "0.1.0"
                "04m15c2067byynyzkpg2fvav86si1vi6671203g7n4v5g4ngdwgd"))

(define rust-ripemd-0.1.3
  (crate-source "ripemd" "0.1.3"
                "17xh5yl9wjjj2v18rh3m8ajlmdjg1yj13l6r9rj3mnbss4i444mx"))

(define rust-roff-0.2.2
  (crate-source "roff" "0.2.2"
                "1wyqz6m0pm4p6wzhwhahvcidfm7nwb38zl4q7ha940pn3w66dy48"))

(define rust-roxmltree-0.20.0
  (crate-source "roxmltree" "0.20.0"
                "15vw91ps91wkmmgy62khf9zb63bdinvm80957dascbsw7dwvc83c"))

(define rust-rsa-0.9.7
  (crate-source "rsa" "0.9.7"
                "06amqm85raq26v6zg00fbf93lbj3kx559n2lpxc3wrvbbiy5vis7"))

(define rust-rusqlite-0.32.1
  (crate-source "rusqlite" "0.32.1"
                "0vlx040bppl414pbjgbp7qr4jdxwszi9krx0m63zzf2f2whvflvp"))

(define rust-rust-fuzzy-search-0.1.1
  (crate-source "rust-fuzzy-search" "0.1.1"
                "1chvl47hq42r219yxs6r1dp4l19acy5ay145hpc5drgzaiq6amx1"))

(define rust-rustc-demangle-0.1.23
  (crate-source "rustc-demangle" "0.1.23"
                "0xnbk2bmyzshacjm2g1kd4zzv2y2az14bw3sjccq5qkpmsfvn9nn"))

(define rust-rustc-demangle-0.1.24
  (crate-source "rustc-demangle" "0.1.24"
                "07zysaafgrkzy2rjgwqdj2a8qdpsm6zv6f5pgpk9x0lm40z9b6vi"))

(define rust-rustc-hash-2.1.1
  (crate-source "rustc-hash" "2.1.1"
                "03gz5lvd9ghcwsal022cgkq67dmimcgdjghfb5yb5d352ga06xrm"))

(define rust-rustc-version-0.4.0
  (crate-source "rustc_version" "0.4.0"
                "0rpk9rcdk405xhbmgclsh4pai0svn49x35aggl4nhbkd4a2zb85z"))

(define rust-rustc-version-0.4.1
  (crate-source "rustc_version" "0.4.1"
                "14lvdsmr5si5qbqzrajgb6vfn69k0sfygrvfvr2mps26xwi3mjyg"))

(define rust-rustix-0.38.10
  (crate-source "rustix" "0.38.10"
                "0r096k86c9sbvirfnz5vy8k8qphkkwahcvi6irqfn9d6rbhlhqpd"))

(define rust-rustix-0.38.43
  (crate-source "rustix" "0.38.43"
                "1xjfhdnmqsbwnfmm77vyh7ldhqx0g9waqm4982404d7jdgp93257"))

(define rust-rustix-0.38.44
  (crate-source "rustix" "0.38.44"
                "0m61v0h15lf5rrnbjhcb9306bgqrhskrqv7i1n0939dsw8dbrdgx"))

(define rust-rustix-1.1.2
  (crate-source "rustix" "1.1.2"
                "0gpz343xfzx16x82s1x336n0kr49j02cvhgxdvaq86jmqnigh5fd"))

(define rust-rustls-0.21.12
  (crate-source "rustls" "0.21.12"
                "0gjdg2a9r81sdwkyw3n5yfbkrr6p9gyk3xr2kcsr3cs83x6s2miz"))

(define rust-rustls-0.22.4
  (crate-source "rustls" "0.22.4"
                "0cl4q6w0x1cl5ldjsgbbiiqhkz6qg5vxl5dkn9wwsyxc44vzfkmz"))

(define rust-rustls-0.23.21
  (crate-source "rustls" "0.23.21"
                "1f75gicypi40bdq20h1rglwaapr3ifnchgf697clkxibc0j7ja4g"))

(define rust-rustls-native-certs-0.7.0
  (crate-source "rustls-native-certs" "0.7.0"
                "14ip15dcr6fmjzi12lla9cpln7mmkdid4a7wsp344v4kz9gbh7wg"))

(define rust-rustls-native-certs-0.7.3
  (crate-source "rustls-native-certs" "0.7.3"
                "1r9ib5gwkfci2wbqnbh44nigvrfgxs4n1x89js82w97dxsab7gz5"))

(define rust-rustls-pemfile-1.0.4
  (crate-source "rustls-pemfile" "1.0.4"
                "1324n5bcns0rnw6vywr5agff3rwfvzphi7rmbyzwnv6glkhclx0w"))

(define rust-rustls-pemfile-2.0.0
  (crate-source "rustls-pemfile" "2.0.0"
                "1x34xidvzn4br2vl8f8xwmhgbjv4lmlb0ggv5whlnk4yl87rir1m"))

(define rust-rustls-pemfile-2.2.0
  (crate-source "rustls-pemfile" "2.2.0"
                "0l3f3mrfkgdjrava7ibwzgwc4h3dljw3pdkbsi9rkwz3zvji9qyw"))

(define rust-rustls-pki-types-1.10.1
  (crate-source "rustls-pki-types" "1.10.1"
                "0dqb3d0cbld1yrp084wyzgw6yk3qzzic8l5pbs1b6bcjzzk4ggyj"))

(define rust-rustls-pki-types-1.7.0
  (crate-source "rustls-pki-types" "1.7.0"
                "0banlc9xzwqrx8n0h4bd0igmq3z5hc72rn941lf22cp3gkkraqlp"))

(define rust-rustls-platform-verifier-0.3.4
  (crate-source "rustls-platform-verifier" "0.3.4"
                "145lhjn9w5khp0fn4lagaa8qqyjyhfqn2dg6llva6qyzvy5qgfxg"))

(define rust-rustls-platform-verifier-0.4.0
  (crate-source "rustls-platform-verifier" "0.4.0"
                "0dnij2d9029i06x4bhyzj5izwv3370233czaqkk1fmgc1wjdrix4"))

(define rust-rustls-platform-verifier-android-0.1.1
  (crate-source "rustls-platform-verifier-android" "0.1.1"
                "13vq6sxsgz9547xm2zbdxiw8x7ad1g8n8ax6xvxsjqszk7q6awgq"))

(define rust-rustls-webpki-0.101.7
  (crate-source "rustls-webpki" "0.101.7"
                "0rapfhpkqp75552i8r0y7f4vq7csb4k7gjjans0df73sxv8paqlb"))

(define rust-rustls-webpki-0.102.4
  (crate-source "rustls-webpki" "0.102.4"
                "0gmk2abk7y2cdppqlaqmnhcv690p19af9n66sjvw84z9j9z8yi7z"))

(define rust-rustls-webpki-0.102.8
  (crate-source "rustls-webpki" "0.102.8"
                "1sdy8ks86b7jpabpnb2px2s7f1sq8v0nqf6fnlvwzm4vfk41pjk4"))

(define rust-rustversion-1.0.14
  (crate-source "rustversion" "1.0.14"
                "1x1pz1yynk5xzzrazk2svmidj69jhz89dz5vrc28sixl20x1iz3z"))

(define rust-rustversion-1.0.19
  (crate-source "rustversion" "1.0.19"
                "1m39qd65jcd1xgqzdm3017ppimiggh2446xngwp1ngr8hjbmpi7p"))

(define rust-rustversion-1.0.22
  (crate-source "rustversion" "1.0.22"
                "0vfl70jhv72scd9rfqgr2n11m5i9l1acnk684m2w83w0zbqdx75k"))

(define rust-rustybuzz-0.20.1
  (crate-source "rustybuzz" "0.20.1"
                "00hp1gwykjfli258zs7lqg8p2zdh94dv2mw8zx7f73m0z2b7qg7x"))

(define rust-ryu-1.0.16
  (crate-source "ryu" "1.0.16"
                "0k7b90xr48ag5bzmfjp82rljasw2fx28xr3bg1lrpx7b5sljm3gr"))

(define rust-ryu-1.0.18
  (crate-source "ryu" "1.0.18"
                "17xx2s8j1lln7iackzd9p0sv546vjq71i779gphjq923vjh5pjzk"))

(define rust-ryu-1.0.20
  (crate-source "ryu" "1.0.20"
                "07s855l8sb333h6bpn24pka5sp7hjk2w667xy6a0khkf6sqv5lr8"))

(define rust-same-file-1.0.6
  (crate-source "same-file" "1.0.6"
                "00h5j1w87dmhnvbv9l8bic3y7xxsnjmssvifw2ayvgx9mb1ivz4k"))

(define rust-scfg-0.3.1
  (crate-source "scfg" "0.3.1"
                "1xfqn2yy75jg0jzwh9x4bxfi575csgrjjym32sf93hhg9nmknf59"))

(define rust-schannel-0.1.22
  (crate-source "schannel" "0.1.22"
                "126zy5jb95fc5hvzyjwiq6lc81r08rdcn6affn00ispp9jzk6dqc"))

(define rust-schannel-0.1.27
  (crate-source "schannel" "0.1.27"
                "0gbbhy28v72kd5iina0z2vcdl3vz63mk5idvkzn5r52z6jmfna8z"))

(define rust-scopeguard-1.2.0
  (crate-source "scopeguard" "1.2.0"
                "0jcz9sd47zlsgcnm1hdw0664krxwb5gczlif4qngj2aif8vky54l"))

(define rust-sct-0.7.1
  (crate-source "sct" "0.7.1"
                "056lmi2xkzdg1dbai6ha3n57s18cbip4pnmpdhyljli3m99n216s"))

(define rust-sec1-0.7.3
  (crate-source "sec1" "0.7.3"
                "1p273j8c87pid6a1iyyc7vxbvifrw55wbxgr0dh3l8vnbxb7msfk"))

(define rust-secret-lib-1.0.0
  (crate-source "secret-lib" "1.0.0"
                "0bwnizb8bpgcqax0i0i7ga3p535dh1pwi6i1xpx584gnvs2v6d5s"))

(define rust-secret-service-4.0.0
  (crate-source "secret-service" "4.0.0"
                "1m5zkmmhg1wv67g4lr6pqjyqg3yrh3b8bgpw1ykf06qqkbcmmlz4"))

(define rust-secular-1.0.1
  (crate-source "secular" "1.0.1"
                "1davw8k29sycm7f4674d4m44jfa7pn812jm3m3mm76srvz63xp63"))

(define rust-security-framework-2.11.1
  (crate-source "security-framework" "2.11.1"
                "00ldclwx78dm61v7wkach9lcx76awlrv0fdgjdwch4dmy12j4yw9"))

(define rust-security-framework-2.9.2
  (crate-source "security-framework" "2.9.2"
                "1pplxk15s5yxvi2m1sz5xfmjibp96cscdcl432w9jzbk0frlzdh5"))

(define rust-security-framework-3.2.0
  (crate-source "security-framework" "3.2.0"
                "05mkrddi9i18h9p098d0iimqv1xxz0wd8mbgpbvh9jj67x0205r7"))

(define rust-security-framework-sys-2.14.0
  ;; TODO: Check bundled sources.
  (crate-source "security-framework-sys" "2.14.0"
                "0chwn01qrnvs59i5220bymd38iddy4krbnmfnhf4k451aqfj7ns9"))

(define rust-security-framework-sys-2.9.1
  ;; TODO: Check bundled sources.
  (crate-source "security-framework-sys" "2.9.1"
                "0yhciwlsy9dh0ps1gw3197kvyqx1bvc4knrhiznhid6kax196cp9"))

(define rust-semver-1.0.21
  (crate-source "semver" "1.0.21"
                "1c49snqlfcx93xym1cgwx8zcspmyyxm37xa2fyfgjx1vhalxfzmr"))

(define rust-semver-1.0.23
  (crate-source "semver" "1.0.23"
                "12wqpxfflclbq4dv8sa6gchdh92ahhwn4ci1ls22wlby3h57wsb1"))

(define rust-semver-1.0.24
  (crate-source "semver" "1.0.24"
                "1fmvjjkd3f64y5fqr1nakkq371mnwzv09fbz5mbmdxril63ypdiw"))

(define rust-serde-1.0.188
  (crate-source "serde" "1.0.188"
                "17jlqzfhimsk8w37ifjwnm86nwjzawlbgwmwc7nhwdwslv5hz7ng"))

(define rust-serde-1.0.196
  (crate-source "serde" "1.0.196"
                "0civrvhbwwk442xhlkfdkkdn478by486qxmackq6k3501zk2c047"))

(define rust-serde-1.0.217
  (crate-source "serde" "1.0.217"
                "0w2ck1p1ajmrv1cf51qf7igjn2nc51r0izzc00fzmmhkvxjl5z02"))

(define rust-serde-1.0.228
  (crate-source "serde" "1.0.228"
                "17mf4hhjxv5m90g42wmlbc61hdhlm6j9hwfkpcnd72rpgzm993ls"))

(define rust-serde-core-1.0.228
  (crate-source "serde_core" "1.0.228"
                "1bb7id2xwx8izq50098s5j2sqrrvk31jbbrjqygyan6ask3qbls1"))

(define rust-serde-derive-1.0.188
  (crate-source "serde_derive" "1.0.188"
                "1wjaclvsfxgqnnnykllvb5gffsxynk66x6h4c1ds6anq8b37mjjf"))

(define rust-serde-derive-1.0.196
  (crate-source "serde_derive" "1.0.196"
                "0rybziqrfaxkaxrybkhrps7zv3ibxnjdk0fwais16zayr5h57j1k"))

(define rust-serde-derive-1.0.217
  (crate-source "serde_derive" "1.0.217"
                "180r3rj5gi5s1m23q66cr5wlfgc5jrs6n1mdmql2njnhk37zg6ss"))

(define rust-serde-derive-1.0.228
  (crate-source "serde_derive" "1.0.228"
                "0y8xm7fvmr2kjcd029g9fijpndh8csv5m20g4bd76w8qschg4h6m"))

(define rust-serde-fmt-1.0.3
  (crate-source "serde_fmt" "1.0.3"
                "190h61yfnzprzb1cvkzmyzcarj52f6xggiz8542xck0h2k5dvm71"))

(define rust-serde-json-1.0.113
  (crate-source "serde_json" "1.0.113"
                "0ycaiff7ar4qx5sy9kvi1kv9rnnfl15kcfmhxiiwknn3n5q1p039"))

(define rust-serde-json-1.0.135
  (crate-source "serde_json" "1.0.135"
                "1n9hcsbxpr2lg5kvlksbcy6bkvqv9rn3hy59600i21kli2i7n39b"))

(define rust-serde-json-1.0.145
  (crate-source "serde_json" "1.0.145"
                "1767y6kxjf7gwpbv8bkhgwc50nhg46mqwm9gy9n122f7v1k6yaj0"))

(define rust-serde-path-to-error-0.1.16
  (crate-source "serde_path_to_error" "0.1.16"
                "19hlz2359l37ifirskpcds7sxg0gzpqvfilibs7whdys0128i6dg"))

(define rust-serde-repr-0.1.19
  (crate-source "serde_repr" "0.1.19"
                "1sb4cplc33z86pzlx38234xr141wr3cmviqgssiadisgl8dlar3c"))

(define rust-serde-spanned-0.6.5
  (crate-source "serde_spanned" "0.6.5"
                "1hgh6s3jjwyzhfk3xwb6pnnr1misq9nflwq0f026jafi37s24dpb"))

(define rust-serde-spanned-0.6.8
  (crate-source "serde_spanned" "0.6.8"
                "1q89g70azwi4ybilz5jb8prfpa575165lmrffd49vmcf76qpqq47"))

(define rust-serde-spanned-0.6.9
  (crate-source "serde_spanned" "0.6.9"
                "18vmxq6qfrm110caszxrzibjhy2s54n1g5w1bshxq9kjmz7y0hdz"))

(define rust-serde-spanned-1.0.2
  (crate-source "serde_spanned" "1.0.2"
                "1vh4kcnzhw0fbr1jhg41p8yybnp5gmpnh171fy25bgn2a8s7h5sl"))

(define rust-serde-toml-merge-0.3.8
  (crate-source "serde-toml-merge" "0.3.8"
                "0s1rbvgwff7x9fl9034s0v5rfjadifdwljc6wkjqkydzsqay9d4k"))

(define rust-serde-xml-rs-0.6.0
  (crate-source "serde-xml-rs" "0.6.0"
                "10i7dvd0c1clj4jbljd08qs8466nlymx7ma7k3ncksx1rn7affpv"))

(define rust-sha1-0.10.6
  (crate-source "sha1" "0.10.6"
                "1fnnxlfg08xhkmwf2ahv634as30l1i3xhlhkvxflmasi5nd85gz3"))

(define rust-sha2-0.10.7
  (crate-source "sha2" "0.10.7"
                "1n3flx8bjyblmb2n860g8402z7q10caajp2n403n37i3cbcbk7s7"))

(define rust-sha2-0.10.8
  (crate-source "sha2" "0.10.8"
                "1j1x78zk9il95w9iv46dh9wm73r6xrgj32y6lzzw7bxws9dbfgbr"))

(define rust-sha3-0.10.8
  (crate-source "sha3" "0.10.8"
                "0q5s3qlwnk8d5j34jya98j1v2p3009wdmnqdza3yydwgi8kjv1vm"))

(define rust-sharded-slab-0.1.7
  (crate-source "sharded-slab" "0.1.7"
                "1xipjr4nqsgw34k7a2cgj9zaasl2ds6jwn89886kww93d32a637l"))

(define rust-shell-words-1.1.0
  (crate-source "shell-words" "1.1.0"
                "1plgwx8r0h5ismbbp6cp03740wmzgzhip85k5hxqrrkaddkql614"))

(define rust-shellexpand-3.1.0
  (crate-source "shellexpand" "3.1.0"
                "0jz1i14ziz8gbyj71212s7dqrw6q96f25i48zkmy66fcjhxzl0ys"))

(define rust-shellexpand-utils-0.2.1
  (crate-source "shellexpand-utils" "0.2.1"
                "1x9rwkrkhdaylw9sh2029bpfn5v3pcb61v20pv8ki6sjjcv8ynn1"))

(define rust-shlex-1.3.0
  (crate-source "shlex" "1.3.0"
                "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg"))

(define rust-signal-hook-0.3.17
  (crate-source "signal-hook" "0.3.17"
                "0098nsah04spqf3n8niirmfym4wsdgjl57c78kmzijlq8xymh8c6"))

(define rust-signal-hook-0.3.18
  (crate-source "signal-hook" "0.3.18"
                "1qnnbq4g2vixfmlv28i1whkr0hikrf1bsc4xjy2aasj2yina30fq"))

(define rust-signal-hook-mio-0.2.4
  (crate-source "signal-hook-mio" "0.2.4"
                "1k8pl9aafiadr4czsg8zal9b4jdk6kq5985p90i19jc5sh31mnrl"))

(define rust-signal-hook-registry-1.4.1
  (crate-source "signal-hook-registry" "1.4.1"
                "18crkkw5k82bvcx088xlf5g4n3772m24qhzgfan80nda7d3rn8nq"))

(define rust-signal-hook-registry-1.4.2
  (crate-source "signal-hook-registry" "1.4.2"
                "1cb5akgq8ajnd5spyn587srvs4n26ryq0p78nswffwhv46sf1sd9"))

(define rust-signal-hook-registry-1.4.6
  (crate-source "signal-hook-registry" "1.4.6"
                "12y2v1ms5z111fymaw1v8k93m5chnkp21h0jknrydkj8zydp395j"))

(define rust-signature-2.2.0
  (crate-source "signature" "2.2.0"
                "1pi9hd5vqfr3q3k49k37z06p7gs5si0in32qia4mmr1dancr6m3p"))

(define rust-simd-adler32-0.3.7
  (crate-source "simd-adler32" "0.3.7"
                "1zkq40c3iajcnr5936gjp9jjh1lpzhy44p3dq3fiw75iwr1w2vfn"))

(define rust-simd-helpers-0.1.0
  (crate-source "simd_helpers" "0.1.0"
                "19idqicn9k4vhd04ifh2ff41wvna79zphdf2c81rlmpc7f3hz2cm"))

(define rust-simple-logger-4.2.0
  (crate-source "simple_logger" "4.2.0"
                "0cwkcbd8ba73ic8g1n896n7mhrgdlm4hnqgvk6vcj5dq55fcsc12"))

(define rust-simplecss-0.2.2
  (crate-source "simplecss" "0.2.2"
                "0v0kid7b2602kcka2x2xs9wwfjf8lnvpgpl8x287qg4wra1ni73s"))

(define rust-siphasher-0.3.11
  (crate-source "siphasher" "0.3.11"
                "03axamhmwsrmh0psdw3gf7c0zc4fyl5yjxfifz9qfka6yhkqid9q"))

(define rust-siphasher-1.0.1
  (crate-source "siphasher" "1.0.1"
                "17f35782ma3fn6sh21c027kjmd227xyrx06ffi8gw4xzv9yry6an"))

(define rust-slab-0.4.9
  (crate-source "slab" "0.4.9"
                "0rxvsgir0qw5lkycrqgb1cxsvxzjv9bmx73bk5y42svnzfba94lg"))

(define rust-sled-0.34.7
  (crate-source "sled" "0.34.7"
                "0dcr2s7cylj5mb33ci3kpx7fz797jwvysnl5airrir9cgirv95kz"))

(define rust-slotmap-1.0.7
  (crate-source "slotmap" "1.0.7"
                "0amqb2fn9lcy1ri0risblkcp88dl0rnfmynw7lx0nqwza77lmzyv"))

(define rust-smallvec-1.13.1
  (crate-source "smallvec" "1.13.1"
                "1mzk9j117pn3k1gabys0b7nz8cdjsx5xc6q7fwnm8r0an62d7v76"))

(define rust-smallvec-1.13.2
  (crate-source "smallvec" "1.13.2"
                "0rsw5samawl3wsw6glrsb127rx6sh89a8wyikicw6dkdcjd1lpiw"))

(define rust-smallvec-1.15.1
  (crate-source "smallvec" "1.15.1"
                "00xxdxxpgyq5vjnpljvkmy99xij5rxgh913ii1v16kzynnivgcb7"))

(define rust-smtp-proto-0.1.5
  (crate-source "smtp-proto" "0.1.5"
                "0fm5cc87hn52cjyjbkkv9xm9v4d90madcamhpbgd9w47s4ysvf2i"))

(define rust-snafu-0.7.5
  (crate-source "snafu" "0.7.5"
                "1mj2j2gfbf8mm1hr02zrbrqrh2zp01f61xgkx0lpln2w0ankgpp4"))

(define rust-snafu-derive-0.7.5
  (crate-source "snafu-derive" "0.7.5"
                "1gzy9rzggs090zf7hfvgp4lm1glrmg9qzh796686jnq7bxk7j04r"))

(define rust-socket2-0.5.5
  (crate-source "socket2" "0.5.5"
                "1sgq315f1njky114ip7wcy83qlphv9qclprfjwvxcpfblmcsqpvv"))

(define rust-socket2-0.5.7
  (crate-source "socket2" "0.5.7"
                "070r941wbq76xpy039an4pyiy3rfj7mp7pvibf1rcri9njq5wc6f"))

(define rust-socket2-0.5.8
  (crate-source "socket2" "0.5.8"
                "1s7vjmb5gzp3iaqi94rh9r63k9cj00kjgbfn7gn60kmnk6fjcw69"))

(define rust-spin-0.9.8
  (crate-source "spin" "0.9.8"
                "0rvam5r0p3a6qhc18scqpvpgb3ckzyqxpgdfyjnghh8ja7byi039"))

(define rust-spki-0.7.3
  (crate-source "spki" "0.7.3"
                "17fj8k5fmx4w9mp27l970clrh5qa7r5sjdvbsln987xhb34dc7nr"))

(define rust-splitty-1.0.2
  (crate-source "splitty" "1.0.2"
                "0ryxmv3yfd96qdc4zasjmqw2r1j62fpnqq2vclfdgr17d0g0mdrd"))

(define rust-sqlite-0.32.0
  (crate-source "sqlite" "0.32.0"
                "1rpqpkpxn2qdvghsnak2b73cn5ca37p6ri0ylyjdcmrq3481r003"))

(define rust-sqlite3-src-0.5.1
  ;; TODO: Check bundled sources.
  (crate-source "sqlite3-src" "0.5.1"
                "0m74wrkpify3z0xvrw4i2yssn9m9sjwqa5ipk6aq6f7fl58mmjdz"))

(define rust-sqlite3-sys-0.15.2
  ;; TODO: Check bundled sources.
  (crate-source "sqlite3-sys" "0.15.2"
                "0fq6m21dnd5yqrzknsmnl2565nahdwa29s7x12xhxr1kjik2qxgj"))

(define rust-stable-deref-trait-1.2.0
  (crate-source "stable_deref_trait" "1.2.0"
                "1lxjr8q2n534b2lhkxd6l6wcddzjvnksi58zv11f9y0jjmr15wd8"))

(define rust-static-assertions-1.1.0
  (crate-source "static_assertions" "1.1.0"
                "0gsl6xmw10gvn3zs1rv99laj5ig7ylffnh71f9l34js4nr4r7sx2"))

(define rust-str-buf-1.0.6
  (crate-source "str-buf" "1.0.6"
                "1l7q4nha7wpsr0970bfqm773vhmpwr9l6rr8r4gwgrh46wvdh24y"))

(define rust-strict-0.2.0
  (crate-source "strict" "0.2.0"
                "01j0h28xzg07kd1km5m0wz88asp6hwh45n8q8bdkjymqlpz4897l"))

(define rust-strict-num-0.1.1
  (crate-source "strict-num" "0.1.1"
                "0cb7l1vhb8zj90mzm8avlk815k40sql9515s865rqdrdfavvldv6"))

(define rust-strsim-0.10.0
  (crate-source "strsim" "0.10.0"
                "08s69r4rcrahwnickvi0kq49z524ci50capybln83mg6b473qivk"))

(define rust-strsim-0.11.1
  (crate-source "strsim" "0.11.1"
                "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))

(define rust-strsim-0.9.3
  (crate-source "strsim" "0.9.3"
                "0k497pv882qn3q977ckznm13vxx927g8s1swvcv68j3c1pccwik4"))

(define rust-strum-0.25.0
  (crate-source "strum" "0.25.0"
                "09g1q55ms8vax1z0mxlbva3vm8n2r1179kfvbccnkjcidzm58399"))

(define rust-strum-0.26.3
  (crate-source "strum" "0.26.3"
                "01lgl6jvrf4j28v5kmx9bp480ygf1nhvac8b4p7rcj9hxw50zv4g"))

(define rust-strum-macros-0.25.3
  (crate-source "strum_macros" "0.25.3"
                "184y62g474zqb2f7n16x3ghvlyjbh50viw32p9w9l5lwmjlizp13"))

(define rust-strum-macros-0.26.4
  (crate-source "strum_macros" "0.26.4"
                "1gl1wmq24b8md527cpyd5bw9rkbqldd7k1h38kf5ajd2ln2ywssc"))

(define rust-subtle-2.5.0
  (crate-source "subtle" "2.5.0"
                "1g2yjs7gffgmdvkkq0wrrh0pxds3q0dv6dhkw9cdpbib656xdkc1"))

(define rust-subtle-2.6.1
  (crate-source "subtle" "2.6.1"
                "14ijxaymghbl1p0wql9cib5zlwiina7kall6w7g89csprkgbvhhk"))

(define rust-sval-2.13.2
  (crate-source "sval" "2.13.2"
                "1n5grsgvdvyf3zdg2nji5v2663r4nplzlgr7fc7b57f462c0zp7n"))

(define rust-sval-buffer-2.13.2
  (crate-source "sval_buffer" "2.13.2"
                "0br1xqldssd1drjxms5p5virk273a3bi97ihsy7yzh23mpvj56a2"))

(define rust-sval-dynamic-2.13.2
  (crate-source "sval_dynamic" "2.13.2"
                "1x0gnmrw8r37fdpn5c1xh3rlhqwpn1cvc680lc8nqf9rv3snzwb8"))

(define rust-sval-fmt-2.13.2
  (crate-source "sval_fmt" "2.13.2"
                "155f9fbpx3823q66j4w1yv2ismyv26ffijwwgxav0lb11fl2f760"))

(define rust-sval-json-2.13.2
  (crate-source "sval_json" "2.13.2"
                "0mgiydngv45jppkn4n36kr2rclxhyh661n39h0m2nqnsf0ygdvhd"))

(define rust-sval-nested-2.13.2
  (crate-source "sval_nested" "2.13.2"
                "17vf2pv22bn3df6m9q9gg12z1m4crzvhqaaxqcabizp1dabyb753"))

(define rust-sval-ref-2.13.2
  (crate-source "sval_ref" "2.13.2"
                "1ymixfmmyvy6qpxrljc32zmgya9m0a9a05lkphlag58pfpinwz5v"))

(define rust-sval-serde-2.13.2
  (crate-source "sval_serde" "2.13.2"
                "0ak5zjfk81pf5m26q30qh8di7gvnsnjy6d1n6jd10s0v6b874m9a"))

(define rust-svg-0.16.0
  (crate-source "svg" "0.16.0"
                "10scx3bxv9sv5jam4aar1bbbrmjsp7ins04phzggxmkg69f1qgjq"))

(define rust-svgtypes-0.15.3
  (crate-source "svgtypes" "0.15.3"
                "1z4a0b76ww6rf2c8zdapqh2a7r7kmmy7m957q5h5ics4zwgm9iv8"))

(define rust-syn-1.0.109
  (crate-source "syn" "1.0.109"
                "0ds2if4600bd59wsv7jjgfkayfzy3hnazs394kz6zdkmna8l3dkj"))

(define rust-syn-2.0.106
  (crate-source "syn" "2.0.106"
                "19mddxp1ia00hfdzimygqmr1jqdvyl86k48427bkci4d08wc9rzd"))

(define rust-syn-2.0.48
  (crate-source "syn" "2.0.48"
                "0gqgfygmrxmp8q32lia9p294kdd501ybn6kn2h4gqza0irik2d8g"))

(define rust-syn-2.0.67
  (crate-source "syn" "2.0.67"
                "144c0apb6zqqlxdyiiy42ynlahn1ddw66cpxwd7azww63pnmb1pz"))

(define rust-syn-2.0.96
  (crate-source "syn" "2.0.96"
                "102wk3cgawimi3i0q3r3xw3i858zkyingg6y7gsxfy733amsvl6m"))

(define rust-synstructure-0.12.6
  (crate-source "synstructure" "0.12.6"
                "03r1lydbf3japnlpc4wka7y90pmz1i0danaj3f9a7b431akdlszk"))

(define rust-synstructure-0.13.1
  (crate-source "synstructure" "0.13.1"
                "0wc9f002ia2zqcbj0q2id5x6n7g1zjqba7qkg2mr0qvvmdk7dby8"))

(define rust-synstructure-0.13.2
  (crate-source "synstructure" "0.13.2"
                "1lh9lx3r3jb18f8sbj29am5hm9jymvbwh6jb1izsnnxgvgrp12kj"))

(define rust-syntect-no-panic-6.0.0
  (crate-source "syntect-no-panic" "6.0.0"
                "1im47kg8jdc70v4xn43wkiz2kl9mgjjznrgnwanspsaxryx8z96m"))

(define rust-system-deps-6.2.0
  (crate-source "system-deps" "6.2.0"
                "0c836abhh3k8yn5ymg8wx383ay7n731gkrbbp3gma352yq7mhb9a"))

(define rust-system-deps-6.2.2
  (crate-source "system-deps" "6.2.2"
                "0j93ryw031n3h8b0nfpj5xwh3ify636xmv8kxianvlyyipmkbrd3"))

(define rust-tagptr-0.2.0
  (crate-source "tagptr" "0.2.0"
                "05r4mwvlsclx1ayj65hpzjv3dn4wpi8j4xm695vydccf9k7r683v"))

(define rust-target-lexicon-0.12.13
  (crate-source "target-lexicon" "0.12.13"
                "1bmgpdq766zn61f16py0x9139fv314d054xkrkj9iw3q5vd8nxb9"))

(define rust-target-lexicon-0.12.16
  (crate-source "target-lexicon" "0.12.16"
                "1cg3bnx1gdkdr5hac1hzxy64fhw4g7dqkd0n3dxy5lfngpr1mi31"))

(define rust-temp-dir-0.1.12
  (crate-source "temp-dir" "0.1.12"
                "1mkizff1mxk4d3hng8cmlggrhvic2dr6fxp3dqf05zhmzsgsl5nx"))

(define rust-tempfile-3.15.0
  (crate-source "tempfile" "3.15.0"
                "016pmkbwn3shas44gcwq1kc9lajalb90qafhiip5fvv8h6f5b2ls"))

(define rust-tempfile-3.23.0
  (crate-source "tempfile" "3.23.0"
                "05igl2gml6z6i2va1bv49f9f1wb3f752c2i63lvlb9s2vxxwfc9d"))

(define rust-tempfile-3.8.0
  (crate-source "tempfile" "3.8.0"
                "1vsl2193w3gpx3mwj36fwx3v6q2qyvmzrdn6m8fgfsjkrkrx556b"))

(define rust-termimad-0.31.3
  (crate-source "termimad" "0.31.3"
                "1dnbqanrhahx6vy94h74vvpqy7ri2cy0vdvnagr9g74kqk1dj0bk"))

(define rust-termimad-0.34.0
  (crate-source "termimad" "0.34.0"
                "084fi8bc6bi1jd7z9jhj1myna6d1mgiv9kb57fj4wpfn8fh5rzv8"))

(define rust-terminal-clipboard-0.4.1
  (crate-source "terminal-clipboard" "0.4.1"
                "0cd125j4wgdi9fy39vmzmiz93zq9g7gjgsspwq0vai7pbk5xh3sf"))

(define rust-terminal-light-1.8.0
  (crate-source "terminal-light" "1.8.0"
                "17zfshypxkyqb8x4srywnxy8894chmas0ljcfv7a0xfq0vlnpxx6"))

(define rust-terminal-size-0.4.1
  (crate-source "terminal_size" "0.4.1"
                "1sd4nq55h9sjirkx0138zx711ddxq1k1a45lc77ninhzj9zl8ljk"))

(define rust-termux-clipboard-0.1.0
  (crate-source "termux-clipboard" "0.1.0"
                "0vz3b2c0ra7x5wg6i56247j8z58wkv3dknznjidk34rjr89zyslz"))

(define rust-thiserror-1.0.47
  (crate-source "thiserror" "1.0.47"
                "13wdsrdyrq6x3rcydvxlx4mxck0c5v3mz1dj8zp7xhdg63n05a4p"))

(define rust-thiserror-1.0.56
  (crate-source "thiserror" "1.0.56"
                "1b9hnzngjan4d89zjs16i01bcpcnvdwklyh73lj16xk28p37hhym"))

(define rust-thiserror-1.0.69
  (crate-source "thiserror" "1.0.69"
                "0lizjay08agcr5hs9yfzzj6axs53a2rgx070a1dsi3jpkcrzbamn"))

(define rust-thiserror-2.0.11
  (crate-source "thiserror" "2.0.11"
                "1z0649rpa8c2smzx129bz4qvxmdihj30r2km6vfpcv9yny2g4lnl"))

(define rust-thiserror-2.0.17
  (crate-source "thiserror" "2.0.17"
                "1j2gixhm2c3s6g96vd0b01v0i0qz1101vfmw0032mdqj1z58fdgn"))

(define rust-thiserror-impl-1.0.47
  (crate-source "thiserror-impl" "1.0.47"
                "16z1irxb45l011af53diap97x44dixnbp60v9g6pvarrdssj7dkb"))

(define rust-thiserror-impl-1.0.56
  (crate-source "thiserror-impl" "1.0.56"
                "0w9ldp8fa574ilz4dn7y7scpcq66vdjy59qal8qdpwsh7faal3zs"))

(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))

(define rust-thiserror-impl-2.0.11
  (crate-source "thiserror-impl" "2.0.11"
                "1hkkn7p2y4cxbffcrprybkj0qy1rl1r6waxmxqvr764axaxc3br6"))

(define rust-thiserror-impl-2.0.17
  (crate-source "thiserror-impl" "2.0.17"
                "04y92yjwg1a4piwk9nayzjfs07sps8c4vq9jnsfq9qvxrn75rw9z"))

(define rust-thread-local-1.1.8
  (crate-source "thread_local" "1.1.8"
                "173i5lyjh011gsimk21np9jn8al18rxsrkjli20a7b8ks2xgk7lb"))

(define rust-tiff-0.10.3
  (crate-source "tiff" "0.10.3"
                "0vrkdk9cdk07rh7iifcxpn6m8zv3wz695mizhr8rb3gfgzg0b5mg"))

(define rust-time-0.3.28
  (crate-source "time" "0.3.28"
                "0j3yl5q4w9vcw55hxxb1a3crls1w82v5dahicj7c4ifjgxavpxhp"))

(define rust-time-0.3.44
  (crate-source "time" "0.3.44"
                "179awlwb36zly3nmz5h9awai1h4pbf1d83g2pmvlw4v1pgixkrwi"))

(define rust-time-core-0.1.1
  (crate-source "time-core" "0.1.1"
                "1yz6d246zbmx9v6wpfg1jyfjlsgagirz7km96pr1mp6snkpzn03k"))

(define rust-time-core-0.1.6
  (crate-source "time-core" "0.1.6"
                "0sqwhg7n47gbffyr0zhipqcnskxgcgzz1ix8wirqs2rg3my8x1j0"))

(define rust-time-macros-0.2.24
  (crate-source "time-macros" "0.2.24"
                "1wzb6hnl35856f58cx259q7ijc4c7yis0qsnydvw5n8jbw9b1krh"))

(define rust-tiny-skia-0.11.4
  (crate-source "tiny-skia" "0.11.4"
                "1aq9gd4qh4418g8v08qzakqqggx8hl66qcianl3k5bjdsja37lc3"))

(define rust-tiny-skia-path-0.11.4
  (crate-source "tiny-skia-path" "0.11.4"
                "14ywbdfakvacl6rxxmzbnycplaxpc6i2linh2yqk0sp8qb07z7lw"))

(define rust-tinystr-0.7.6
  (crate-source "tinystr" "0.7.6"
                "0bxqaw7z8r2kzngxlzlgvld1r6jbnwyylyvyjbv1q71rvgaga5wi"))

(define rust-tinystr-0.8.1
  (crate-source "tinystr" "0.8.1"
                "12sc6h3hnn6x78iycm5v6wrs2xhxph0ydm43yyn7gdfw8l8nsksx"))

(define rust-tinyvec-1.10.0
  (crate-source "tinyvec" "1.10.0"
                "1yhk0qdqyiaa4v2j9h8pzax5gxgwpz4da0lcphfil6g6pk1zv9dz"))

(define rust-tinyvec-1.8.1
  (crate-source "tinyvec" "1.8.1"
                "1s41rv7n39sjsxz3kd3d4adw45ndkxz1d18rfbz2wd7s9n8bhb82"))

(define rust-tinyvec-macros-0.1.1
  (crate-source "tinyvec_macros" "0.1.1"
                "081gag86208sc3y6sdkshgw3vysm5d34p431dzw0bshz66ncng0z"))

(define rust-tokio-1.36.0
  (crate-source "tokio" "1.36.0"
                "0c89p36zbd4abr1z3l5mipp43x7z4c9b4vp4s6r8y0gs2mjmya31"))

(define rust-tokio-1.41.0
  (crate-source "tokio" "1.41.0"
                "1fwb4nm630hmy9cyl2ar6wxqckgvsakwhg1rhjza4is3a09k8pql"))

(define rust-tokio-1.43.0
  (crate-source "tokio" "1.43.0"
                "17pdm49ihlhfw3rpxix3kdh2ppl1yv7nwp1kxazi5r1xz97zlq9x"))

(define rust-tokio-macros-2.2.0
  (crate-source "tokio-macros" "2.2.0"
                "0fwjy4vdx1h9pi4g2nml72wi0fr27b5m954p13ji9anyy8l1x2jv"))

(define rust-tokio-macros-2.4.0
  (crate-source "tokio-macros" "2.4.0"
                "0lnpg14h1v3fh2jvnc8cz7cjf0m7z1xgkwfpcyy632g829imjgb9"))

(define rust-tokio-macros-2.5.0
  (crate-source "tokio-macros" "2.5.0"
                "1f6az2xbvqp7am417b78d1za8axbvjvxnmkakz9vr8s52czx81kf"))

(define rust-tokio-rustls-0.24.1
  (crate-source "tokio-rustls" "0.24.1"
                "10bhibg57mqir7xjhb2xmf24xgfpx6fzpyw720a4ih8a737jg0y2"))

(define rust-tokio-rustls-0.25.0
  (crate-source "tokio-rustls" "0.25.0"
                "03w6d5aqqf084rmcmrsyq5grhydl53blaiqcl0i2yfnv187hqpkp"))

(define rust-tokio-rustls-0.26.1
  (crate-source "tokio-rustls" "0.26.1"
                "0dxz4bhkn4bwnvzjqvqlg70ba5fslnmf9r6yr87wzq5cx9shjvaz"))

(define rust-tokio-stream-0.1.14
  (crate-source "tokio-stream" "0.1.14"
                "0hi8hcwavh5sdi1ivc9qc4yvyr32f153c212dpd7sb366y6rhz1r"))

(define rust-tokio-util-0.7.10
  (crate-source "tokio-util" "0.7.10"
                "058y6x4mf0fsqji9rfyb77qbfyc50y4pk2spqgj6xsyr693z66al"))

(define rust-toml-0.8.19
  (crate-source "toml" "0.8.19"
                "0knjd3mkxyb87qcs2dark3qkpadidap3frqfj5nqvhpxwfc1zvd1"))

(define rust-toml-0.8.2
  (crate-source "toml" "0.8.2"
                "0g9ysjaqvm2mv8q85xpqfn7hi710hj24sd56k49wyddvvyq8lp8q"))

(define rust-toml-0.8.23
  (crate-source "toml" "0.8.23"
                "0qnkrq4lm2sdhp3l6cb6f26i8zbnhqb7mhbmksd550wxdfcyn6yw"))

(define rust-toml-0.9.7
  (crate-source "toml" "0.9.7"
                "187av4nsjc0cdfixpc24sqpxqwy5ijvdm7hd9yfsqx94pzcybr80"))

(define rust-toml-datetime-0.6.11
  (crate-source "toml_datetime" "0.6.11"
                "077ix2hb1dcya49hmi1avalwbixmrs75zgzb3b2i7g2gizwdmk92"))

(define rust-toml-datetime-0.6.3
  (crate-source "toml_datetime" "0.6.3"
                "0jsy7v8bdvmzsci6imj8fzgd255fmy5fzp6zsri14yrry7i77nkw"))

(define rust-toml-datetime-0.6.8
  (crate-source "toml_datetime" "0.6.8"
                "0hgv7v9g35d7y9r2afic58jvlwnf73vgd1mz2k8gihlgrf73bmqd"))

(define rust-toml-datetime-0.7.2
  (crate-source "toml_datetime" "0.7.2"
                "1hgff8gdk9yx7dljkqfijmj0sc5ln4xhpj045divdhi7xifhiw9j"))

(define rust-toml-edit-0.19.15
  (crate-source "toml_edit" "0.19.15"
                "08bl7rp5g6jwmfpad9s8jpw8wjrciadpnbaswgywpr9hv9qbfnqv"))

(define rust-toml-edit-0.20.2
  (crate-source "toml_edit" "0.20.2"
                "0f7k5svmxw98fhi28jpcyv7ldr2s3c867pjbji65bdxjpd44svir"))

(define rust-toml-edit-0.22.22
  (crate-source "toml_edit" "0.22.22"
                "1xf7sxfzmnc45f75x302qrn5aph52vc8w226v59yhrm211i8vr2a"))

(define rust-toml-edit-0.22.27
  (crate-source "toml_edit" "0.22.27"
                "16l15xm40404asih8vyjvnka9g0xs9i4hfb6ry3ph9g419k8rzj1"))

(define rust-toml-parser-1.0.3
  (crate-source "toml_parser" "1.0.3"
                "09x6i0b57lwc7yn6w1kbd2ypm4vpcrgd2vdax7h745g77g1r7y2c"))

(define rust-toml-writer-1.0.3
  (crate-source "toml_writer" "1.0.3"
                "0281l7bgchmlbvxmci01p9x2w5br9p61ylns5ji65rbc24yacqyi"))

(define rust-tower-0.4.13
  (crate-source "tower" "0.4.13"
                "073wncyqav4sak1p755hf6vl66njgfc1z1g1di9rxx3cvvh9pymq"))

(define rust-tower-0.5.1
  (crate-source "tower" "0.5.1"
                "0kvbp97bhb4sk24vhihcz74ngn0i4ygxqikmxndgng3w926r6wr8"))

(define rust-tower-http-0.6.2
  (crate-source "tower-http" "0.6.2"
                "15wnvhl6cpir9125s73bqjzjsvfb0fmndmsimnl2ddnlhfvs6gs0"))

(define rust-tower-layer-0.3.3
  (crate-source "tower-layer" "0.3.3"
                "03kq92fdzxin51w8iqix06dcfgydyvx7yr6izjq0p626v9n2l70j"))

(define rust-tower-service-0.3.3
  (crate-source "tower-service" "0.3.3"
                "1hzfkvkci33ra94xjx64vv3pp0sq346w06fpkcdwjcid7zhvdycd"))

(define rust-tracing-0.1.40
  (crate-source "tracing" "0.1.40"
                "1vv48dac9zgj9650pg2b4d0j3w6f3x9gbggf43scq5hrlysklln3"))

(define rust-tracing-0.1.41
  (crate-source "tracing" "0.1.41"
                "1l5xrzyjfyayrwhvhldfnwdyligi1mpqm8mzbi2m1d6y6p2hlkkq"))

(define rust-tracing-attributes-0.1.27
  (crate-source "tracing-attributes" "0.1.27"
                "1rvb5dn9z6d0xdj14r403z0af0bbaqhg02hq4jc97g5wds6lqw1l"))

(define rust-tracing-attributes-0.1.28
  (crate-source "tracing-attributes" "0.1.28"
                "0v92l9cxs42rdm4m5hsa8z7ln1xsiw1zc2iil8c6k7lzq0jf2nir"))

(define rust-tracing-core-0.1.32
  (crate-source "tracing-core" "0.1.32"
                "0m5aglin3cdwxpvbg6kz0r9r0k31j48n0kcfwsp6l49z26k3svf0"))

(define rust-tracing-core-0.1.33
  (crate-source "tracing-core" "0.1.33"
                "170gc7cxyjx824r9kr17zc9gvzx89ypqfdzq259pr56gg5bwjwp6"))

(define rust-tracing-core-0.1.34
  (crate-source "tracing-core" "0.1.34"
                "0y3nc4mpnr79rzkrcylv5f5bnjjp19lsxwis9l4kzs97ya0jbldr"))

(define rust-tracing-error-0.2.1
  (crate-source "tracing-error" "0.2.1"
                "1nzk6qcvhmxxy3lw1nj71anmfmvxlnk78l5lym1389vs1l1825cb"))

(define rust-tracing-log-0.2.0
  (crate-source "tracing-log" "0.2.0"
                "1hs77z026k730ij1a9dhahzrl0s073gfa2hm5p0fbl0b80gmz1gf"))

(define rust-tracing-subscriber-0.3.19
  (crate-source "tracing-subscriber" "0.3.19"
                "0220rignck8072i89jjsh140vmh14ydwpdwnifyaf3xcnpn9s678"))

(define rust-trash-5.2.3
  (crate-source "trash" "5.2.3"
                "1wsgyriw10wl4h46pgs589laq4vwh8q5ma4aflwpl08j212k98v5"))

(define rust-tree-magic-mini-3.1.6
  (crate-source "tree_magic_mini" "3.1.6"
                "0qwx2b0xfr00vdskl951cvh3m040zj5n8vm7ln4k6p143ybyiida"))

(define rust-triomphe-0.1.11
  (crate-source "triomphe" "0.1.11"
                "1crf71hndy3fc68x8v4aikkdjynp4n5sdhq28sck8x7frx8bd7l5"))

(define rust-try-lock-0.2.4
  (crate-source "try-lock" "0.2.4"
                "1vc15paa4zi06ixsxihwbvfn24d708nsyg1ncgqwcrn42byyqa1m"))

(define rust-ttf-parser-0.25.1
  (crate-source "ttf-parser" "0.25.1"
                "0cbgqglcwwjg3hirwq6xlza54w04mb5x02kf7zx4hrw50xmr1pyj"))

(define rust-twofish-0.7.1
  (crate-source "twofish" "0.7.1"
                "04w0ii2c0c9ws08aw6c7illh9zql22il9lbwjk1mgir30aiq73m7"))

(define rust-typeid-1.0.2
  (crate-source "typeid" "1.0.2"
                "0vi32jv3s3nbybbl4r317wi2bk8j4fx4d8p88jji8pnd1hpdn4qf"))

(define rust-typenum-1.16.0
  (crate-source "typenum" "1.16.0"
                "1fhb9iaqyjn4dzn2vl86kxjhp4xpw5gynczlnqzf4x6rjgpn2ya9"))

(define rust-typenum-1.17.0
  (crate-source "typenum" "1.17.0"
                "09dqxv69m9lj9zvv6xw5vxaqx15ps0vxyy5myg33i0kbqvq0pzs2"))

(define rust-uds-windows-1.1.0
  (crate-source "uds_windows" "1.1.0"
                "1fb4y65pw0rsp0gyfyinjazlzxz1f6zv7j4zmb20l5pxwv1ypnl9"))

(define rust-umask-2.1.0
  (crate-source "umask" "2.1.0"
                "071xszsd6znk0ik11pxl7mwhf07clsiq3qpzw1ac0dcyak14d6pc"))

(define rust-unicase-2.8.1
  (crate-source "unicase" "2.8.1"
                "0fd5ddbhpva7wrln2iah054ar2pc1drqjcll0f493vj3fv8l9f3m"))

(define rust-unicode-bidi-0.3.18
  (crate-source "unicode-bidi" "0.3.18"
                "1xcxwbsqa24b8vfchhzyyzgj0l6bn51ib5v8j6krha0m77dva72w"))

(define rust-unicode-bidi-mirroring-0.4.0
  (crate-source "unicode-bidi-mirroring" "0.4.0"
                "1zirs1z3ahlwy7swg7apnm3pc6vix1g15q0kn6fx8rmvc266xyjx"))

(define rust-unicode-ccc-0.4.0
  (crate-source "unicode-ccc" "0.4.0"
                "0gjhxwx27ywm3rcbb0m5q20w8zxi51440b3ps6swi6ywpj4d8qff"))

(define rust-unicode-ident-1.0.11
  (crate-source "unicode-ident" "1.0.11"
                "0g7wmn39nl9yzhjwn9ihacd22ymli8r4nlc2xf3idaas8ypbl6ih"))

(define rust-unicode-ident-1.0.12
  (crate-source "unicode-ident" "1.0.12"
                "0jzf1znfpb2gx8nr8mvmyqs1crnv79l57nxnbiszc7xf7ynbjm1k"))

(define rust-unicode-ident-1.0.14
  (crate-source "unicode-ident" "1.0.14"
                "10ywa1pg0glgkr4l3dppjxizr9r2b7im0ycbfa0137l69z5fdfdd"))

(define rust-unicode-ident-1.0.19
  (crate-source "unicode-ident" "1.0.19"
                "17bx1j1zf6b9j3kpyf74mraary7ava3984km0n8kh499h5a58fpn"))

(define rust-unicode-normalization-0.1.24
  (crate-source "unicode-normalization" "0.1.24"
                "0mnrk809z3ix1wspcqy97ld5wxdb31f3xz6nsvg5qcv289ycjcsh"))

(define rust-unicode-properties-0.1.3
  (crate-source "unicode-properties" "0.1.3"
                "1l3mbgzwz8g14xcs09p4ww3hjkjcf0i1ih13nsg72bhj8n5jl3z7"))

(define rust-unicode-script-0.5.7
  (crate-source "unicode-script" "0.5.7"
                "07vwr9iddw5xwrj57hc6ig0mwmlzjdajj9lyfxqz9by9a2rj3d4z"))

(define rust-unicode-segmentation-1.12.0
  (crate-source "unicode-segmentation" "1.12.0"
                "14qla2jfx74yyb9ds3d2mpwpa4l4lzb9z57c6d2ba511458z5k7n"))

(define rust-unicode-vo-0.1.0
  (crate-source "unicode-vo" "0.1.0"
                "151sha088v9jyfvbg5164xh4dk72g53b82xm4zzbf5dlagzqdlxi"))

(define rust-unicode-width-0.1.14
  (crate-source "unicode-width" "0.1.14"
                "1bzn2zv0gp8xxbxbhifw778a7fc93pa6a1kj24jgg9msj07f7mkx"))

(define rust-unicode-width-0.2.0
  (crate-source "unicode-width" "0.2.0"
                "1zd0r5vs52ifxn25rs06gxrgz8cmh4xpra922k0xlmrchib1kj0z"))

(define rust-unicode-width-0.2.1
  (crate-source "unicode-width" "0.2.1"
                "0k0mlq7xy1y1kq6cgv1r2rs2knn6rln3g3af50rhi0dkgp60f6ja"))

(define rust-unicode-xid-0.2.4
  (crate-source "unicode-xid" "0.2.4"
                "131dfzf7d8fsr1ivch34x42c2d1ik5ig3g78brxncnn0r1sdyqpr"))

(define rust-untrusted-0.9.0
  (crate-source "untrusted" "0.9.0"
                "1ha7ib98vkc538x0z60gfn0fc5whqdd85mb87dvisdcaifi6vjwf"))

(define rust-ureq-3.0.0-rc5
  (crate-source "ureq" "3.0.0-rc5"
                "0wzi4mih0ksjhxj0m39zzyy5qlr2kvha05dij1yghfzy0cfvvybp"))

(define rust-ureq-proto-0.2.3
  (crate-source "ureq-proto" "0.2.3"
                "1cwyr58lzlhhnj32r349s0f9rwni3giir49ak546yhhm5kyjjxx2"))

(define rust-url-2.5.4
  (crate-source "url" "2.5.4"
                "0q6sgznyy2n4l5lm16zahkisvc9nip9aa5q1pps7656xra3bdy1j"))

(define rust-url-2.5.7
  (crate-source "url" "2.5.7"
                "0nzghdv0kcksyvri0npxbjzyx2ihprks5k590y77bld355m17g08"))

(define rust-urlencoding-2.1.3
  (crate-source "urlencoding" "2.1.3"
                "1nj99jp37k47n0hvaz5fvz7z6jd0sb4ppvfy3nphr1zbnyixpy6s"))

(define rust-usvg-0.45.1
  (crate-source "usvg" "0.45.1"
                "1vs7gfhyxjgkgibgwfjniwrszfw0iivj1aq06hq8nfxfzc39pgl0"))

(define rust-utf-8-0.7.6
  (crate-source "utf-8" "0.7.6"
                "1a9ns3fvgird0snjkd3wbdhwd3zdpc2h5gpyybrfr6ra5pkqxk09"))

(define rust-utf16-iter-1.0.5
  (crate-source "utf16_iter" "1.0.5"
                "0ik2krdr73hfgsdzw0218fn35fa09dg2hvbi1xp3bmdfrp9js8y8"))

(define rust-utf7-imap-0.3.2
  (crate-source "utf7-imap" "0.3.2"
                "0pczz3pazxj6ypnz1n5v4w24llqh501vdpq910gpdhhz4rjn6cjy"))

(define rust-utf8-iter-1.0.4
  (crate-source "utf8_iter" "1.0.4"
                "1gmna9flnj8dbyd8ba17zigrp9c4c3zclngf5lnb5yvz1ri41hdn"))

(define rust-utf8parse-0.2.2
  (crate-source "utf8parse" "0.2.2"
                "088807qwjq46azicqwbhlmzwrbkz7l4hpw43sdkdyyk524vdxaq6"))

(define rust-uuid-0.8.2
  (crate-source "uuid" "0.8.2"
                "1dy4ldcp7rnzjy56dxh7d2sgrcvn4q77y0a8r0a48946h66zjp5w"))

(define rust-uuid-1.11.0
  (crate-source "uuid" "1.11.0"
                "0sj4l28lif2wm4xrafdfgqjywjzv43wzp8nii9a4i539myhg1igq"))

(define rust-uuid-1.11.1
  (crate-source "uuid" "1.11.1"
                "1i2nlxkzfxsi0bz33z28ny4szk0r8fv65k33klk2w544zsss64xr"))

(define rust-uuid-1.7.0
  (crate-source "uuid" "1.7.0"
                "0aivp5ys7sg2izlj2sn6rr8p43vdcwg64naj8n0kqbd15iqcj37h"))

(define rust-uzers-0.12.1
  (crate-source "uzers" "0.12.1"
                "1pcpi9v90nr3q2y3i4pkac9c20r1nzaimvcm7vajmn770ksizy2d"))

(define rust-v-frame-0.3.9
  (crate-source "v_frame" "0.3.9"
                "1qkvb4ks33zck931vzqckjn36hkngj6l2cwmvfsnlpc7r0kpfsv6"))

(define rust-valuable-0.1.0
  (crate-source "valuable" "0.1.0"
                "0v9gp3nkjbl30z0fd56d8mx7w1csk86wwjhfjhr400wh9mfpw2w3"))

(define rust-value-bag-1.10.0
  (crate-source "value-bag" "1.10.0"
                "1lnsixdpi1ldms1adxyafyx7lyrqxhhskgwrjckmml6majmc9x1y"))

(define rust-value-bag-serde1-1.10.0
  (crate-source "value-bag-serde1" "1.10.0"
                "02sv7k5q2gf9ga91fj3k2rj3hrlw9m2r8v1kdv5cfngx6syp7dsb"))

(define rust-value-bag-sval2-1.10.0
  (crate-source "value-bag-sval2" "1.10.0"
                "0fm8lkzbf18x8hch9dqlrmxncxcp8iynb5scd643mi6a0akidaak"))

(define rust-vcpkg-0.2.15
  (crate-source "vcpkg" "0.2.15"
                "09i4nf5y8lig6xgj3f7fyrvzd3nlaw4znrihw8psidvv5yk4xkdc"))

(define rust-version-check-0.9.4
  (crate-source "version_check" "0.9.4"
                "0gs8grwdlgh0xq660d7wr80x14vxbizmd8dbp29p2pdncx8lp1s9"))

(define rust-version-check-0.9.5
  (crate-source "version_check" "0.9.5"
                "0nhhi4i5x89gm911azqbn7avs9mdacw2i3vcz3cnmz3mv4rqz4hb"))

(define rust-version-compare-0.1.1
  (crate-source "version-compare" "0.1.1"
                "0acg4pmjdbmclg0m7yhijn979mdy66z3k8qrcnvn634f1gy456jp"))

(define rust-version-compare-0.2.0
  (crate-source "version-compare" "0.2.0"
                "12y9262fhjm1wp0aj3mwhads7kv0jz8h168nn5fb8b43nwf9abl5"))

(define rust-vparser-1.0.1
  (crate-source "vparser" "1.0.1"
                "0yjszxiqz9bwxd5qx4w8k1gcbgf1mi9wrk75d89443najyl3klzr"))

(define rust-vte-0.15.0
  (crate-source "vte" "0.15.0"
                "1g9xgnw7q7zdwgfqa6zfcfsp92wn0j0h13kzsqy0dq3c80c414m5"))

(define rust-walkdir-2.5.0
  (crate-source "walkdir" "2.5.0"
                "0jsy7a710qv8gld5957ybrnc07gavppp963gs32xk4ag8130jy99"))

(define rust-want-0.3.1
  (crate-source "want" "0.3.1"
                "03hbfrnvqqdchb5kgxyavb9jabwza0dmh2vw5kg0dq8rxl57d9xz"))

(define rust-wasi-0.11.0+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.0+wasi-snapshot-preview1"
                "08z4hxwkpdpalxjps1ai9y7ihin26y9f476i53dv98v45gkqg3cw"))

(define rust-wasi-0.11.1+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.1+wasi-snapshot-preview1"
                "0jx49r7nbkbhyfrfyhz0bm4817yrnxgd3jiwwwfv0zl439jyrwyc"))

(define rust-wasi-0.14.7+wasi-0.2.4
  (crate-source "wasi" "0.14.7+wasi-0.2.4"
                "133fq3mq7h65mzrsphcm7bbbx1gsz7srrbwh01624zin43g7hd48"))

(define rust-wasip2-1.0.1+wasi-0.2.4
  (crate-source "wasip2" "1.0.1+wasi-0.2.4"
                "1rsqmpspwy0zja82xx7kbkbg9fv34a4a2if3sbd76dy64a244qh5"))

(define rust-wasm-bindgen-0.2.104
  (crate-source "wasm-bindgen" "0.2.104"
                "0b8f4l6pqm0bz0lj5xgwmchb6977n71vmh7srd0axwg93b011nn1"))

(define rust-wasm-bindgen-0.2.95
  (crate-source "wasm-bindgen" "0.2.95"
                "0bpbvmxhil380gpv53smaypl8wc7sy7rq8apxfw349pn78v1x38j"))

(define rust-wasm-bindgen-0.2.99
  (crate-source "wasm-bindgen" "0.2.99"
                "15k3rzb3kjrxyqnh0916gq99mrpwhwy62smawxxc2w0x3llgcx54"))

(define rust-wasm-bindgen-backend-0.2.104
  (crate-source "wasm-bindgen-backend" "0.2.104"
                "069vnhhn2j4w2gwd8rch6g8d3iwkrgi45fas6i3qm7glcrd9l737"))

(define rust-wasm-bindgen-backend-0.2.95
  (crate-source "wasm-bindgen-backend" "0.2.95"
                "0n53wgy78bgzgjwk0z69zbspzhv8p2a4zh69s4fzvpqdrb9x8vfb"))

(define rust-wasm-bindgen-backend-0.2.99
  (crate-source "wasm-bindgen-backend" "0.2.99"
                "0ycwa4c68j34687k513djgyy2asn3fw3yp4g9rkq2kvbchwbp2az"))

(define rust-wasm-bindgen-macro-0.2.104
  (crate-source "wasm-bindgen-macro" "0.2.104"
                "06d1m5bg272h6jabq0snm7c50fifjz6r20f5hqlmz7y5wivh99kw"))

(define rust-wasm-bindgen-macro-0.2.95
  (crate-source "wasm-bindgen-macro" "0.2.95"
                "0mic8b2vab1a91m6x3hjxkwz23094bq1cwhnszarsnlggyz894z7"))

(define rust-wasm-bindgen-macro-0.2.99
  (crate-source "wasm-bindgen-macro" "0.2.99"
                "1znlcrk5bvisr3vscwlqkdby959n3sb367zgdzpjwjd7v4giiiic"))

(define rust-wasm-bindgen-macro-support-0.2.104
  (crate-source "wasm-bindgen-macro-support" "0.2.104"
                "1mr18kx7ima1pmsqlkk982q4a0vf3r8s1x6901jb59sd1prd41wz"))

(define rust-wasm-bindgen-macro-support-0.2.95
  (crate-source "wasm-bindgen-macro-support" "0.2.95"
                "0s7g6glb85lyx2pj83shbmg4d50mvqhb2c2qk2j28yigaxbspii6"))

(define rust-wasm-bindgen-macro-support-0.2.99
  (crate-source "wasm-bindgen-macro-support" "0.2.99"
                "1hihsgyg0kf46kjhgfv8x5g9x0q1d0aizj6n7s84ag1xfrdskmrh"))

(define rust-wasm-bindgen-shared-0.2.104
  (crate-source "wasm-bindgen-shared" "0.2.104"
                "1la1xj9v3gmawnlyi7lc3mb3xi447r6frb98hi2fb9m1nb47vmms"))

(define rust-wasm-bindgen-shared-0.2.95
  (crate-source "wasm-bindgen-shared" "0.2.95"
                "1386q7mvv5ky003hcc6yyxpid3y1m7fy0l920i3z3ab60vqhkz35"))

(define rust-wasm-bindgen-shared-0.2.99
  (crate-source "wasm-bindgen-shared" "0.2.99"
                "19h61snrhh1qhb5gz6zyb89l7fbj1fhmxcvi09p9l0mav8zsnfll"))

(define rust-web-sys-0.3.72
  ;; TODO: Check bundled sources.
  (crate-source "web-sys" "0.3.72"
                "04k19hilj9r8sx6q20fz853149gfpmf83yk2zvq0s14c2288nj7n"))

(define rust-webpki-root-certs-0.26.7
  (crate-source "webpki-root-certs" "0.26.7"
                "0p15xwdlibwqlmkqjb6qqikypyxqb0lwxf70rxa01wzipm4xmmcw"))

(define rust-webpki-roots-0.26.7
  (crate-source "webpki-roots" "0.26.7"
                "0zpykqqk4jnrx55jc8wcysnprhfdcwh35dsiwhm2fybydgqjyr2x"))

(define rust-weezl-0.1.10
  (crate-source "weezl" "0.1.10"
                "1wqnxqn8n90bgazs6djlibf58ppdxki4slblwp9lgnq0fwkv6ld7"))

(define rust-which-4.4.2
  (crate-source "which" "4.4.2"
                "1ixzmx3svsv5hbdvd8vdhd3qwvf6ns8jdpif1wmwsy10k90j9fl7"))

(define rust-widestring-1.1.0
  (crate-source "widestring" "1.1.0"
                "048kxd6iykzi5la9nikpc5hvpp77hmjf1sw43sl3z2dcdrmx66bj"))

(define rust-winapi-0.3.9
  (crate-source "winapi" "0.3.9"
                "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))

(define rust-winapi-i686-pc-windows-gnu-0.4.0
  (crate-source "winapi-i686-pc-windows-gnu" "0.4.0"
                "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))

(define rust-winapi-util-0.1.11
  (crate-source "winapi-util" "0.1.11"
                "08hdl7mkll7pz8whg869h58c1r9y7in0w0pk8fm24qc77k0b39y2"))

(define rust-winapi-util-0.1.9
  (crate-source "winapi-util" "0.1.9"
                "1fqhkcl9scd230cnfj8apfficpf5c9vhwnk4yy9xfc1sw69iq8ng"))

(define rust-winapi-wsapoll-0.1.2
  (crate-source "winapi-wsapoll" "0.1.2"
                "0a1zxmpvxaw75y4lwavi6qbq95cnrz83a5p84rarjxn5g7vcbbqy"))

(define rust-winapi-x86-64-pc-windows-gnu-0.4.0
  (crate-source "winapi-x86_64-pc-windows-gnu" "0.4.0"
                "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))

(define rust-windows-0.56.0
  (crate-source "windows" "0.56.0"
                "0cp10nzrqgrlk91dpwxjcpzyy6imr5vxr5f898pss7nz3gq9vrhx"))

(define rust-windows-aarch64-gnullvm-0.42.2
  (crate-source "windows_aarch64_gnullvm" "0.42.2"
                "1y4q0qmvl0lvp7syxvfykafvmwal5hrjb4fmv04bqs0bawc52yjr"))

(define rust-windows-aarch64-gnullvm-0.48.5
  (crate-source "windows_aarch64_gnullvm" "0.48.5"
                "1n05v7qblg1ci3i567inc7xrkmywczxrs1z3lj3rkkxw18py6f1b"))

(define rust-windows-aarch64-gnullvm-0.52.6
  (crate-source "windows_aarch64_gnullvm" "0.52.6"
                "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"))

(define rust-windows-aarch64-gnullvm-0.53.0
  (crate-source "windows_aarch64_gnullvm" "0.53.0"
                "0r77pbpbcf8bq4yfwpz2hpq3vns8m0yacpvs2i5cn6fx1pwxbf46"))

(define rust-windows-aarch64-msvc-0.42.2
  (crate-source "windows_aarch64_msvc" "0.42.2"
                "0hsdikjl5sa1fva5qskpwlxzpc5q9l909fpl1w6yy1hglrj8i3p0"))

(define rust-windows-aarch64-msvc-0.48.5
  (crate-source "windows_aarch64_msvc" "0.48.5"
                "1g5l4ry968p73g6bg6jgyvy9lb8fyhcs54067yzxpcpkf44k2dfw"))

(define rust-windows-aarch64-msvc-0.52.6
  (crate-source "windows_aarch64_msvc" "0.52.6"
                "0sfl0nysnz32yyfh773hpi49b1q700ah6y7sacmjbqjjn5xjmv09"))

(define rust-windows-aarch64-msvc-0.53.0
  (crate-source "windows_aarch64_msvc" "0.53.0"
                "0v766yqw51pzxxwp203yqy39ijgjamp54hhdbsyqq6x1c8gilrf7"))

(define rust-windows-core-0.52.0
  (crate-source "windows-core" "0.52.0"
                "1nc3qv7sy24x0nlnb32f7alzpd6f72l4p24vl65vydbyil669ark"))

(define rust-windows-core-0.56.0
  (crate-source "windows-core" "0.56.0"
                "19pj57bm0rzhlk0ghrccd3i5zvh0ghm52f8cmdc8d3yhs8pfb626"))

(define rust-windows-core-0.62.1
  (crate-source "windows-core" "0.62.1"
                "1aa94x61q0x39xnlzxjmahwck9i5p51xgzrz7m6hi1dj2rafwi38"))

(define rust-windows-i686-gnu-0.42.2
  (crate-source "windows_i686_gnu" "0.42.2"
                "0kx866dfrby88lqs9v1vgmrkk1z6af9lhaghh5maj7d4imyr47f6"))

(define rust-windows-i686-gnu-0.48.5
  (crate-source "windows_i686_gnu" "0.48.5"
                "0gklnglwd9ilqx7ac3cn8hbhkraqisd0n83jxzf9837nvvkiand7"))

(define rust-windows-i686-gnu-0.52.6
  (crate-source "windows_i686_gnu" "0.52.6"
                "02zspglbykh1jh9pi7gn8g1f97jh1rrccni9ivmrfbl0mgamm6wf"))

(define rust-windows-i686-gnu-0.53.0
  (crate-source "windows_i686_gnu" "0.53.0"
                "1hvjc8nv95sx5vdd79fivn8bpm7i517dqyf4yvsqgwrmkmjngp61"))

(define rust-windows-i686-gnullvm-0.52.6
  (crate-source "windows_i686_gnullvm" "0.52.6"
                "0rpdx1537mw6slcpqa0rm3qixmsb79nbhqy5fsm3q2q9ik9m5vhf"))

(define rust-windows-i686-gnullvm-0.53.0
  (crate-source "windows_i686_gnullvm" "0.53.0"
                "04df1in2k91qyf1wzizvh560bvyzq20yf68k8xa66vdzxnywrrlw"))

(define rust-windows-i686-msvc-0.42.2
  (crate-source "windows_i686_msvc" "0.42.2"
                "0q0h9m2aq1pygc199pa5jgc952qhcnf0zn688454i7v4xjv41n24"))

(define rust-windows-i686-msvc-0.48.5
  (crate-source "windows_i686_msvc" "0.48.5"
                "01m4rik437dl9rdf0ndnm2syh10hizvq0dajdkv2fjqcywrw4mcg"))

(define rust-windows-i686-msvc-0.52.6
  (crate-source "windows_i686_msvc" "0.52.6"
                "0rkcqmp4zzmfvrrrx01260q3xkpzi6fzi2x2pgdcdry50ny4h294"))

(define rust-windows-i686-msvc-0.53.0
  (crate-source "windows_i686_msvc" "0.53.0"
                "0pcvb25fkvqnp91z25qr5x61wyya12lx8p7nsa137cbb82ayw7sq"))

(define rust-windows-implement-0.56.0
  (crate-source "windows-implement" "0.56.0"
                "16rgkvlx4syqmajfdwmkcvn6nvh126wjj8sg3jvsk5fdivskbz7n"))

(define rust-windows-implement-0.60.1
  (crate-source "windows-implement" "0.60.1"
                "1q2lfwdqrkfzsrlshvvyr2cj7ckq4rqxj0ispzlnvyvl5bj0gczd"))

(define rust-windows-interface-0.56.0
  (crate-source "windows-interface" "0.56.0"
                "1k2prfxna0mw47f8gi8qhw9jfpw66bh2cqzs67sgipjfpx30b688"))

(define rust-windows-interface-0.59.2
  (crate-source "windows-interface" "0.59.2"
                "19a6if8dfnazjgjw4hm0kayk9vrjclyj3iqivcaaqr39pkfx3ay0"))

(define rust-windows-link-0.2.0
  (crate-source "windows-link" "0.2.0"
                "0r9w2z96d5phmm185aq92z54jp9h2nqisa4wgc71idxbc436rr25"))

(define rust-windows-result-0.1.2
  (crate-source "windows-result" "0.1.2"
                "1y274q1v0vy21lhkgslpxpq1m08hvr1mcs2l88h1b1gcx0136f2y"))

(define rust-windows-result-0.4.0
  (crate-source "windows-result" "0.4.0"
                "0zqn8kmmf7y9yw9g7q6pbcg9dbry9m03fqi0b92q767q0v1xr13h"))

(define rust-windows-strings-0.5.0
  (crate-source "windows-strings" "0.5.0"
                "1nld65azvms87rdm2bdm8gskwdmsswh4pxbc8babxc2klmawc63j"))

(define rust-windows-sys-0.42.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.42.0"
                "19waf8aryvyq9pzk0gamgfwjycgzk4gnrazpfvv171cby0h1hgjs"))

(define rust-windows-sys-0.48.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.48.0"
                "1aan23v5gs7gya1lc46hqn9mdh8yph3fhxmhxlw36pn6pqc28zb7"))

(define rust-windows-sys-0.52.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.52.0"
                "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))

(define rust-windows-sys-0.59.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.59.0"
                "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y"))

(define rust-windows-sys-0.60.2
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.60.2"
                "1jrbc615ihqnhjhxplr2kw7rasrskv9wj3lr80hgfd42sbj01xgj"))

(define rust-windows-sys-0.61.1
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.61.1"
                "03vg2rxm0lyiyq64b5sm95lkg2x95sjdb0zb0y4q8g2avm0rw43g"))

(define rust-windows-targets-0.48.5
  (crate-source "windows-targets" "0.48.5"
                "034ljxqshifs1lan89xwpcy1hp0lhdh4b5n0d2z4fwjx2piacbws"))

(define rust-windows-targets-0.52.6
  (crate-source "windows-targets" "0.52.6"
                "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))

(define rust-windows-targets-0.53.4
  (crate-source "windows-targets" "0.53.4"
                "0jxc6f032xb3bbb7mj9rlhky84w7vz7hkbsh8s2hcakdysvvfhid"))

(define rust-windows-x86-64-gnu-0.42.2
  (crate-source "windows_x86_64_gnu" "0.42.2"
                "0dnbf2xnp3xrvy8v9mgs3var4zq9v9yh9kv79035rdgyp2w15scd"))

(define rust-windows-x86-64-gnu-0.48.5
  (crate-source "windows_x86_64_gnu" "0.48.5"
                "13kiqqcvz2vnyxzydjh73hwgigsdr2z1xpzx313kxll34nyhmm2k"))

(define rust-windows-x86-64-gnu-0.52.6
  (crate-source "windows_x86_64_gnu" "0.52.6"
                "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"))

(define rust-windows-x86-64-gnu-0.53.0
  (crate-source "windows_x86_64_gnu" "0.53.0"
                "1flh84xkssn1n6m1riddipydcksp2pdl45vdf70jygx3ksnbam9f"))

(define rust-windows-x86-64-gnullvm-0.42.2
  (crate-source "windows_x86_64_gnullvm" "0.42.2"
                "18wl9r8qbsl475j39zvawlidp1bsbinliwfymr43fibdld31pm16"))

(define rust-windows-x86-64-gnullvm-0.48.5
  (crate-source "windows_x86_64_gnullvm" "0.48.5"
                "1k24810wfbgz8k48c2yknqjmiigmql6kk3knmddkv8k8g1v54yqb"))

(define rust-windows-x86-64-gnullvm-0.52.6
  (crate-source "windows_x86_64_gnullvm" "0.52.6"
                "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"))

(define rust-windows-x86-64-gnullvm-0.53.0
  (crate-source "windows_x86_64_gnullvm" "0.53.0"
                "0mvc8119xpbi3q2m6mrjcdzl6afx4wffacp13v76g4jrs1fh6vha"))

(define rust-windows-x86-64-msvc-0.42.2
  (crate-source "windows_x86_64_msvc" "0.42.2"
                "1w5r0q0yzx827d10dpjza2ww0j8iajqhmb54s735hhaj66imvv4s"))

(define rust-windows-x86-64-msvc-0.48.5
  (crate-source "windows_x86_64_msvc" "0.48.5"
                "0f4mdp895kkjh9zv8dxvn4pc10xr7839lf5pa9l0193i2pkgr57d"))

(define rust-windows-x86-64-msvc-0.52.6
  (crate-source "windows_x86_64_msvc" "0.52.6"
                "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq"))

(define rust-windows-x86-64-msvc-0.53.0
  (crate-source "windows_x86_64_msvc" "0.53.0"
                "11h4i28hq0zlnjcaqi2xdxr7ibnpa8djfggch9rki1zzb8qi8517"))

(define rust-winnow-0.5.39
  (crate-source "winnow" "0.5.39"
                "0a9d8yxfnlgfd27samr5ld6hlpx7vr4qzxpqfy6d50qnn1aa32ak"))

(define rust-winnow-0.6.24
  (crate-source "winnow" "0.6.24"
                "0fm0z1gk9wb47s1jhh889isz657kavd1yb3fhzbjmi657icimmy8"))

(define rust-winnow-0.7.13
  (crate-source "winnow" "0.7.13"
                "1krrjc1wj2vx0r57m9nwnlc1zrhga3fq41d8w9hysvvqb5mj7811"))

(define rust-winreg-0.10.1
  (crate-source "winreg" "0.10.1"
                "17c6h02z88ijjba02bnxi5k94q5cz490nf3njh9yypf8fbig9l40"))

(define rust-winreg-0.50.0
  (crate-source "winreg" "0.50.0"
                "1cddmp929k882mdh6i9f2as848f13qqna6czwsqzkh1pqnr5fkjj"))

(define rust-winreg-0.52.0
  (crate-source "winreg" "0.52.0"
                "19gh9vp7mp1ab84kc3ag48nm9y7xgjhh3xa4vxss1gylk1rsaxx2"))

(define rust-wit-bindgen-0.46.0
  (crate-source "wit-bindgen" "0.46.0"
                "0ngysw50gp2wrrfxbwgp6dhw1g6sckknsn3wm7l00vaf7n48aypi"))

(define rust-write16-1.0.0
  (crate-source "write16" "1.0.0"
                "0dnryvrrbrnl7vvf5vb1zkmwldhjkf2n5znliviam7bm4900z2fi"))

(define rust-writeable-0.5.5
  (crate-source "writeable" "0.5.5"
                "0lawr6y0bwqfyayf3z8zmqlhpnzhdx0ahs54isacbhyjwa7g778y"))

(define rust-writeable-0.6.1
  (crate-source "writeable" "0.6.1"
                "1fx29zncvbrqzgz7li88vzdm8zvgwgwy2r9bnjqxya09pfwi0bza"))

(define rust-x11-clipboard-0.8.1
  (crate-source "x11-clipboard" "0.8.1"
                "1ps0fk1912vzy382fc8l926q8w1l8bxmw72l3kr9bwdi2l8wl6ml"))

(define rust-x11rb-0.12.0
  (crate-source "x11rb" "0.12.0"
                "02h492k920mb1733cdmly138zfiwkspil6ssqcvi7inyshk1nr5i"))

(define rust-x11rb-protocol-0.12.0
  (crate-source "x11rb-protocol" "0.12.0"
                "1g24qdvq0mbyl2npz7zqy5v6hpdxq2qakkpnp3x02rzvl3ww7ml2"))

(define rust-x25519-dalek-2.0.1
  (crate-source "x25519-dalek" "2.0.1"
                "0xyjgqpsa0q6pprakdp58q1hy45rf8wnqqscgzx0gyw13hr6ir67"))

(define rust-xdg-home-1.3.0
  (crate-source "xdg-home" "1.3.0"
                "1xm122zz0wjc8p8cmchij0j9nw34hwncb39jc7dc0mgvb2rdl77c"))

(define rust-xml-rs-0.8.25
  (crate-source "xml-rs" "0.8.25"
                "1i73ajf6scni5bi1a51r19xykgrambdx5fkks0fyg5jqqbml1ff5"))

(define rust-xmlwriter-0.1.0
  (crate-source "xmlwriter" "0.1.0"
                "1fg0ldmkgiis6hnxpi1c9gy7v23y0lpi824bp8yp12fi3r82lypc"))

(define rust-xterm-query-0.5.2
  (crate-source "xterm-query" "0.5.2"
                "0cak14nd70qd99vrrikzaqlks6k8l7gyrbx7hz6lxpjg8ggk6b19"))

(define rust-yaml-rust-0.4.5
  (crate-source "yaml-rust" "0.4.5"
                "118wbqrr4n6wgk5rjjnlrdlahawlxc1bdsx146mwk8f79in97han"))

(define rust-yansi-0.5.1
  (crate-source "yansi" "0.5.1"
                "1v4qljgzh73knr7291cgwrf56zrvhmpn837n5n5pypzq1kciq109"))

(define rust-yoke-0.7.5
  (crate-source "yoke" "0.7.5"
                "0h3znzrdmll0a7sglzf9ji0p5iqml11wrj1dypaf6ad6kbpnl3hj"))

(define rust-yoke-0.8.0
  (crate-source "yoke" "0.8.0"
                "1k4mfr48vgi7wh066y11b7v1ilakghlnlhw9snzz8vi2p00vnhaz"))

(define rust-yoke-derive-0.7.5
  (crate-source "yoke-derive" "0.7.5"
                "0m4i4a7gy826bfvnqa9wy6sp90qf0as3wps3wb0smjaamn68g013"))

(define rust-yoke-derive-0.8.0
  (crate-source "yoke-derive" "0.8.0"
                "1dha5jrjz9jaq8kmxq1aag86b98zbnm9lyjrihy5sv716sbkrniq"))

(define rust-z-base-32-0.1.4
  (crate-source "z-base-32" "0.1.4"
                "0fqk028mzvgnx9phqzvhzrx3fbzv4r736cm3x3hid136g157pgr1"))

(define rust-zbus-4.4.0
  (crate-source "zbus" "4.4.0"
                "09f7916lp7haxv1y5zgcg99ny15whi6dn3waf1afcafxx8mh35xv"))

(define rust-zbus-macros-4.4.0
  (crate-source "zbus_macros" "4.4.0"
                "0glqn6ddgv4ra734p343a41rrxb0phy1v13dljzhpsc1f10bjz96"))

(define rust-zbus-names-3.0.0
  (crate-source "zbus_names" "3.0.0"
                "0v1f0ajwafj47bf11yp0xdgp26r93lslr9nb2v6624h2gppiz6sb"))

(define rust-zerocopy-0.7.35
  (crate-source "zerocopy" "0.7.35"
                "1w36q7b9il2flg0qskapgi9ymgg7p985vniqd09vi0mwib8lz6qv"))

(define rust-zerocopy-0.8.27
  (crate-source "zerocopy" "0.8.27"
                "0b1870gf2zzlckca69v2k4mqwmf8yh2li37qldnzvvd3by58g508"))

(define rust-zerocopy-derive-0.7.35
  (crate-source "zerocopy-derive" "0.7.35"
                "0gnf2ap2y92nwdalzz3x7142f2b83sni66l39vxp2ijd6j080kzs"))

(define rust-zerocopy-derive-0.8.27
  (crate-source "zerocopy-derive" "0.8.27"
                "0c9qrylm2p55dvaplxsl24ma48add9qk4y0d6kjbkllaqvcvill8"))

(define rust-zerofrom-0.1.5
  (crate-source "zerofrom" "0.1.5"
                "0bnd8vjcllzrvr3wvn8x14k2hkrpyy1fm3crkn2y3plmr44fxwyg"))

(define rust-zerofrom-0.1.6
  (crate-source "zerofrom" "0.1.6"
                "19dyky67zkjichsb7ykhv0aqws3q0jfvzww76l66c19y6gh45k2h"))

(define rust-zerofrom-derive-0.1.5
  (crate-source "zerofrom-derive" "0.1.5"
                "022q55phhb44qbrcfbc48k0b741fl8gnazw3hpmmndbx5ycfspjr"))

(define rust-zerofrom-derive-0.1.6
  (crate-source "zerofrom-derive" "0.1.6"
                "00l5niw7c1b0lf1vhvajpjmcnbdp2vn96jg4nmkhq2db0rp5s7np"))

(define rust-zeroize-1.7.0
  (crate-source "zeroize" "1.7.0"
                "0bfvby7k9pdp6623p98yz2irqnamcyzpn7zh20nqmdn68b0lwnsj"))

(define rust-zeroize-1.8.1
  (crate-source "zeroize" "1.8.1"
                "1pjdrmjwmszpxfd7r860jx54cyk94qk59x13sc307cvr5256glyf"))

(define rust-zeroize-derive-1.4.2
  (crate-source "zeroize_derive" "1.4.2"
                "0sczjlqjdmrp3wn62g7mw6p438c9j4jgp2f9zamd56991mdycdnf"))

(define rust-zerotrie-0.2.2
  (crate-source "zerotrie" "0.2.2"
                "15gmka7vw5k0d24s0vxgymr2j6zn2iwl12wpmpnpjgsqg3abpw1n"))

(define rust-zerovec-0.10.4
  (crate-source "zerovec" "0.10.4"
                "0yghix7n3fjfdppwghknzvx9v8cf826h2qal5nqvy8yzg4yqjaxa"))

(define rust-zerovec-0.11.4
  (crate-source "zerovec" "0.11.4"
                "0fz7j1ns8d86m2fqg2a4bzi5gnh5892bxv4kcr9apwc6a3ajpap7"))

(define rust-zerovec-derive-0.10.3
  (crate-source "zerovec-derive" "0.10.3"
                "1ik322dys6wnap5d3gcsn09azmssq466xryn5czfm13mn7gsdbvf"))

(define rust-zerovec-derive-0.11.1
  (crate-source "zerovec-derive" "0.11.1"
                "13zms8hj7vzpfswypwggyfr4ckmyc7v3di49pmj8r1qcz9z275jv"))

(define rust-zune-bmp-0.4.0
  (crate-source "zune-bmp" "0.4.0"
                "03wcrvc3a5s2154bdp07mrsxghjpmc2nabghzh1a4jrlhj6ns79v"))

(define rust-zune-core-0.4.12
  (crate-source "zune-core" "0.4.12"
                "0jj1ra86klzlcj9aha9als9d1dzs7pqv3azs1j3n96822wn3lhiz"))

(define rust-zune-farbfeld-0.4.0
  (crate-source "zune-farbfeld" "0.4.0"
                "04gh23s72hz8gz80phk0ljf427ksi4hj8qx88hh5d72lh3569a7f"))

(define rust-zune-hdr-0.4.0
  (crate-source "zune-hdr" "0.4.0"
                "09k4d043n7dj99x5zy3arpkjvv82hyvvminvqspi82zq6534gzsi"))

(define rust-zune-image-0.4.15
  (crate-source "zune-image" "0.4.15"
                "0q829p18hrpqm04wr27zgrdvi6274qzv7xw8dr051in81yn9hsrx"))

(define rust-zune-inflate-0.2.54
  (crate-source "zune-inflate" "0.2.54"
                "00kg24jh3zqa3i6rg6yksnb71bch9yi1casqydl00s7nw8pk7avk"))

(define rust-zune-jpeg-0.4.21
  (crate-source "zune-jpeg" "0.4.21"
                "04r7g6y9jp7d4c9bq23rz3gwzlr1dsl7vdk4yly35bc4jf52rki9"))

(define rust-zune-jpegxl-0.4.0
  (crate-source "zune-jpegxl" "0.4.0"
                "0x1nkrxil53d3nxayflc3wzdd1m17zdvys27xncmzcc48d4fxzvi"))

(define rust-zune-png-0.4.10
  (crate-source "zune-png" "0.4.10"
                "02bmqybbma3b94m9sy6wbwvikh0a2a9z1458zvpjjvwwfs2w0abx"))

(define rust-zune-ppm-0.4.0
  (crate-source "zune-ppm" "0.4.0"
                "0x179gpzf85qkrw0dy81bgqm43z6asqv8xm51aqk8fry4d2yc9q5"))

(define rust-zune-psd-0.4.0
  (crate-source "zune-psd" "0.4.0"
                "0jadhgyjk0i3s5v347671wy77c7a8z34w3y2limj04qqc9fjf4db"))

(define rust-zune-qoi-0.4.10
  (crate-source "zune-qoi" "0.4.10"
                "1jg9hhj15zjwc8ycrmanl5an1d43vf6ps1xm7i0ldrgd55kqnnfw"))

(define rust-zvariant-4.2.0
  (crate-source "zvariant" "4.2.0"
                "1zl1ika7zd9bxkd0bqc78h9bykvk6xc98965iz1p3i51p452k110"))

(define rust-zvariant-derive-4.2.0
  (crate-source "zvariant_derive" "4.2.0"
                "0jf408h0s83krxwm7wl62fnssin1kcklmb1bcn83ls6sddabmqkk"))

(define rust-zvariant-utils-2.1.0
  (crate-source "zvariant_utils" "2.1.0"
                "0h43h3jcw8rmjr390rdqnhkb9nn3913pgkvb75am1frxrkvwy6y5"))

(define-cargo-inputs lookup-cargo-inputs
                     (broot =>
                            (list rust-adler2-2.0.1
                                  rust-ahash-0.8.12
                                  rust-aho-corasick-1.1.3
                                  rust-aligned-vec-0.6.4
                                  rust-allocator-api2-0.2.21
                                  rust-android-system-properties-0.1.5
                                  rust-ansi-colours-1.2.3
                                  rust-anstream-0.6.20
                                  rust-anstyle-1.0.13
                                  rust-anstyle-parse-0.2.7
                                  rust-anstyle-query-1.1.4
                                  rust-anstyle-wincon-3.0.10
                                  rust-anyhow-1.0.100
                                  rust-arbitrary-1.4.2
                                  rust-arg-enum-proc-macro-0.3.4
                                  rust-argh-0.1.13
                                  rust-argh-derive-0.1.13
                                  rust-argh-shared-0.1.13
                                  rust-arrayref-0.3.9
                                  rust-arrayvec-0.7.6
                                  rust-autocfg-1.5.0
                                  rust-av1-grain-0.2.4
                                  rust-avif-serialize-0.8.6
                                  rust-base64-0.13.1
                                  rust-base64-0.22.1
                                  rust-bet-1.0.4
                                  rust-bincode-1.3.3
                                  rust-bit-set-0.5.3
                                  rust-bit-vec-0.6.3
                                  rust-bit-field-0.10.3
                                  rust-bitflags-1.3.2
                                  rust-bitflags-2.9.4
                                  rust-bitstream-io-2.6.0
                                  rust-block2-0.5.1
                                  rust-bstr-1.12.0
                                  rust-built-0.7.7
                                  rust-bumpalo-3.19.0
                                  rust-bytemuck-1.23.2
                                  rust-byteorder-lite-0.1.0
                                  rust-cc-1.2.39
                                  rust-cfg-expr-0.15.8
                                  rust-cfg-if-1.0.3
                                  rust-cfg-aliases-0.2.1
                                  rust-char-reader-0.1.1
                                  rust-chrono-0.4.42
                                  rust-clap-4.5.48
                                  rust-clap-help-1.5.0
                                  rust-clap-builder-4.5.48
                                  rust-clap-complete-4.5.58
                                  rust-clap-derive-4.5.47
                                  rust-clap-lex-0.7.5
                                  rust-clap-mangen-0.2.29
                                  rust-cli-log-2.1.0
                                  rust-clipboard-win-4.5.0
                                  rust-clipboard-macos-0.1.1
                                  rust-color-quant-1.1.0
                                  rust-colorchoice-1.0.4
                                  rust-convert-case-0.7.1
                                  rust-coolor-1.1.0
                                  rust-core-foundation-0.9.4
                                  rust-core-foundation-sys-0.8.7
                                  rust-core-maths-0.1.1
                                  rust-crc32fast-1.5.0
                                  rust-crokey-1.3.0
                                  rust-crokey-proc-macros-1.3.0
                                  rust-crossbeam-0.8.4
                                  rust-crossbeam-channel-0.5.15
                                  rust-crossbeam-deque-0.8.6
                                  rust-crossbeam-epoch-0.9.18
                                  rust-crossbeam-queue-0.3.12
                                  rust-crossbeam-utils-0.8.21
                                  rust-crossterm-0.28.1
                                  rust-crossterm-0.29.0
                                  rust-crossterm-winapi-0.9.1
                                  rust-crunchy-0.2.4
                                  rust-csv-1.3.1
                                  rust-csv-core-0.1.12
                                  rust-csv2svg-0.2.3
                                  rust-custom-error-1.9.2
                                  rust-data-url-0.3.2
                                  rust-deranged-0.5.4
                                  rust-derive-more-2.0.1
                                  rust-derive-more-impl-2.0.1
                                  rust-deser-hjson-2.2.4
                                  rust-directories-4.0.1
                                  rust-directories-5.0.1
                                  rust-dirs-sys-0.3.7
                                  rust-dirs-sys-0.4.1
                                  rust-displaydoc-0.2.5
                                  rust-doc-comment-0.3.3
                                  rust-document-features-0.2.11
                                  rust-either-1.15.0
                                  rust-equator-0.4.2
                                  rust-equator-macro-0.4.2
                                  rust-equivalent-1.0.2
                                  rust-errno-0.3.14
                                  rust-error-code-2.3.1
                                  rust-euclid-0.22.11
                                  rust-exr-1.73.0
                                  rust-fallible-iterator-0.3.0
                                  rust-fallible-streaming-iterator-0.1.9
                                  rust-fancy-regex-0.11.0
                                  rust-fastrand-2.3.0
                                  rust-fax-0.2.6
                                  rust-fax-derive-0.2.0
                                  rust-fdeflate-0.3.7
                                  rust-file-size-1.0.3
                                  rust-find-msvc-tools-0.1.2
                                  rust-flate2-1.1.2
                                  rust-flex-grow-0.1.0
                                  rust-float-cmp-0.9.0
                                  rust-fnv-1.0.7
                                  rust-foldhash-0.1.5
                                  rust-fontconfig-parser-0.5.8
                                  rust-fontdb-0.23.0
                                  rust-form-urlencoded-1.2.2
                                  rust-fsevent-sys-4.1.0
                                  rust-gethostname-0.3.0
                                  rust-getrandom-0.2.16
                                  rust-getrandom-0.3.3
                                  rust-gif-0.13.3
                                  rust-git2-0.20.2
                                  rust-glassbench-0.4.4
                                  rust-glob-0.3.3
                                  rust-half-2.6.0
                                  rust-hashbrown-0.14.5
                                  rust-hashbrown-0.15.5
                                  rust-hashbrown-0.16.0
                                  rust-hashlink-0.9.1
                                  rust-heck-0.4.1
                                  rust-heck-0.5.0
                                  rust-home-0.5.11
                                  rust-iana-time-zone-0.1.64
                                  rust-iana-time-zone-haiku-0.1.2
                                  rust-icu-collections-2.0.0
                                  rust-icu-locale-core-2.0.0
                                  rust-icu-normalizer-2.0.0
                                  rust-icu-normalizer-data-2.0.0
                                  rust-icu-properties-2.0.1
                                  rust-icu-properties-data-2.0.1
                                  rust-icu-provider-2.0.0
                                  rust-id-arena-2.2.1
                                  rust-idna-1.1.0
                                  rust-idna-adapter-1.2.1
                                  rust-image-0.25.8
                                  rust-image-webp-0.2.4
                                  rust-imagesize-0.13.0
                                  rust-imgref-1.12.0
                                  rust-include-dir-0.7.4
                                  rust-include-dir-macros-0.7.4
                                  rust-indexmap-2.11.4
                                  rust-inotify-0.11.0
                                  rust-inotify-sys-0.1.5
                                  rust-interpolate-name-0.2.4
                                  rust-io-kit-sys-0.4.1
                                  rust-is-executable-1.0.5
                                  rust-is-terminal-polyfill-1.70.1
                                  rust-itertools-0.12.1
                                  rust-itoa-1.0.15
                                  rust-jobserver-0.1.34
                                  rust-jpeg-encoder-0.5.1
                                  rust-js-sys-0.3.81
                                  rust-jxl-bitstream-0.2.3
                                  rust-jxl-coding-0.2.3
                                  rust-jxl-color-0.3.2
                                  rust-jxl-frame-0.5.0
                                  rust-jxl-grid-0.1.1
                                  rust-jxl-image-0.5.0
                                  rust-jxl-modular-0.3.0
                                  rust-jxl-oxide-0.4.0
                                  rust-jxl-render-0.4.0
                                  rust-jxl-vardct-0.3.0
                                  rust-kamadak-exif-0.5.5
                                  rust-kqueue-1.1.1
                                  rust-kqueue-sys-1.0.4
                                  rust-kurbo-0.11.3
                                  rust-lazy-regex-3.4.1
                                  rust-lazy-regex-proc-macros-3.4.1
                                  rust-lazy-static-1.5.0
                                  rust-lebe-0.5.3
                                  rust-lfs-core-0.14.1
                                  rust-libc-0.2.176
                                  rust-libfuzzer-sys-0.4.10
                                  rust-libgit2-sys-0.18.2+1.9.1
                                  rust-libm-0.2.15
                                  rust-libredox-0.1.10
                                  rust-libsqlite3-sys-0.30.1
                                  rust-libz-sys-1.1.22
                                  rust-linked-hash-map-0.5.6
                                  rust-linux-raw-sys-0.4.15
                                  rust-linux-raw-sys-0.11.0
                                  rust-litemap-0.8.0
                                  rust-litrs-0.4.2
                                  rust-lock-api-0.4.13
                                  rust-log-0.4.28
                                  rust-loop9-0.1.5
                                  rust-lru-0.16.1
                                  rust-mach2-0.4.3
                                  rust-maybe-rayon-0.1.1
                                  rust-memchr-2.7.6
                                  rust-memmap2-0.9.8
                                  rust-memoffset-0.7.1
                                  rust-minimad-0.13.1
                                  rust-minimal-lexical-0.2.1
                                  rust-miniz-oxide-0.8.9
                                  rust-mio-1.0.4
                                  rust-moxcms-0.7.5
                                  rust-mutate-once-0.1.2
                                  rust-new-debug-unreachable-1.0.6
                                  rust-nix-0.26.4
                                  rust-nix-0.29.0
                                  rust-nom-7.1.3
                                  rust-noop-proc-macro-0.3.0
                                  rust-normpath-1.5.0
                                  rust-notify-8.2.0
                                  rust-notify-types-2.0.0
                                  rust-num-bigint-0.4.6
                                  rust-num-conv-0.1.0
                                  rust-num-derive-0.4.2
                                  rust-num-integer-0.1.46
                                  rust-num-rational-0.4.2
                                  rust-num-traits-0.2.19
                                  rust-objc-sys-0.3.5
                                  rust-objc2-0.5.2
                                  rust-objc2-0.6.2
                                  rust-objc2-app-kit-0.2.2
                                  rust-objc2-core-data-0.2.2
                                  rust-objc2-core-image-0.2.2
                                  rust-objc2-encode-4.1.0
                                  rust-objc2-foundation-0.2.2
                                  rust-objc2-foundation-0.3.1
                                  rust-objc2-metal-0.2.2
                                  rust-objc2-quartz-core-0.2.2
                                  rust-once-cell-1.21.3
                                  rust-once-cell-polyfill-1.70.1
                                  rust-open-1.7.1
                                  rust-opener-0.8.3
                                  rust-option-ext-0.2.0
                                  rust-parking-lot-0.12.4
                                  rust-parking-lot-core-0.9.11
                                  rust-paste-1.0.15
                                  rust-pathdiff-0.2.3
                                  rust-percent-encoding-2.3.2
                                  rust-phf-0.13.1
                                  rust-phf-generator-0.13.1
                                  rust-phf-macros-0.13.1
                                  rust-phf-shared-0.13.1
                                  rust-pico-args-0.5.0
                                  rust-pin-project-lite-0.2.16
                                  rust-pkg-config-0.3.32
                                  rust-plist-1.8.0
                                  rust-png-0.17.16
                                  rust-png-0.18.0
                                  rust-potential-utf-0.1.3
                                  rust-powerfmt-0.2.0
                                  rust-ppv-lite86-0.2.21
                                  rust-proc-macro2-1.0.101
                                  rust-proc-status-0.1.1
                                  rust-profiling-1.0.17
                                  rust-profiling-procmacros-1.0.17
                                  rust-pxfm-0.1.24
                                  rust-qoi-0.4.1
                                  rust-quick-error-2.0.1
                                  rust-quick-xml-0.38.3
                                  rust-quote-1.0.41
                                  rust-r-efi-5.3.0
                                  rust-rand-0.8.5
                                  rust-rand-chacha-0.3.1
                                  rust-rand-core-0.6.4
                                  rust-rav1e-0.7.1
                                  rust-ravif-0.11.20
                                  rust-rayon-1.11.0
                                  rust-rayon-core-1.13.0
                                  rust-redox-syscall-0.5.17
                                  rust-redox-users-0.4.6
                                  rust-regex-1.11.3
                                  rust-regex-automata-0.4.11
                                  rust-regex-syntax-0.8.6
                                  rust-resvg-0.45.1
                                  rust-rgb-0.8.52
                                  rust-roff-0.2.2
                                  rust-roxmltree-0.20.0
                                  rust-rusqlite-0.32.1
                                  rust-rust-fuzzy-search-0.1.1
                                  rust-rustc-hash-2.1.1
                                  rust-rustix-0.38.44
                                  rust-rustix-1.1.2
                                  rust-rustversion-1.0.22
                                  rust-rustybuzz-0.20.1
                                  rust-ryu-1.0.20
                                  rust-same-file-1.0.6
                                  rust-scopeguard-1.2.0
                                  rust-secular-1.0.1
                                  rust-serde-1.0.228
                                  rust-serde-core-1.0.228
                                  rust-serde-derive-1.0.228
                                  rust-serde-json-1.0.145
                                  rust-serde-spanned-0.6.9
                                  rust-serde-spanned-1.0.2
                                  rust-shlex-1.3.0
                                  rust-signal-hook-0.3.18
                                  rust-signal-hook-mio-0.2.4
                                  rust-signal-hook-registry-1.4.6
                                  rust-simd-adler32-0.3.7
                                  rust-simd-helpers-0.1.0
                                  rust-simplecss-0.2.2
                                  rust-siphasher-1.0.1
                                  rust-slotmap-1.0.7
                                  rust-smallvec-1.15.1
                                  rust-snafu-0.7.5
                                  rust-snafu-derive-0.7.5
                                  rust-splitty-1.0.2
                                  rust-stable-deref-trait-1.2.0
                                  rust-str-buf-1.0.6
                                  rust-strict-0.2.0
                                  rust-strict-num-0.1.1
                                  rust-strsim-0.11.1
                                  rust-svg-0.16.0
                                  rust-svgtypes-0.15.3
                                  rust-syn-1.0.109
                                  rust-syn-2.0.106
                                  rust-synstructure-0.13.2
                                  rust-syntect-no-panic-6.0.0
                                  rust-system-deps-6.2.2
                                  rust-target-lexicon-0.12.16
                                  rust-tempfile-3.23.0
                                  rust-termimad-0.31.3
                                  rust-termimad-0.34.0
                                  rust-terminal-clipboard-0.4.1
                                  rust-terminal-light-1.8.0
                                  rust-termux-clipboard-0.1.0
                                  rust-thiserror-1.0.69
                                  rust-thiserror-2.0.17
                                  rust-thiserror-impl-1.0.69
                                  rust-thiserror-impl-2.0.17
                                  rust-tiff-0.10.3
                                  rust-time-0.3.44
                                  rust-time-core-0.1.6
                                  rust-time-macros-0.2.24
                                  rust-tiny-skia-0.11.4
                                  rust-tiny-skia-path-0.11.4
                                  rust-tinystr-0.8.1
                                  rust-tinyvec-1.10.0
                                  rust-tinyvec-macros-0.1.1
                                  rust-toml-0.8.23
                                  rust-toml-0.9.7
                                  rust-toml-datetime-0.6.11
                                  rust-toml-datetime-0.7.2
                                  rust-toml-edit-0.22.27
                                  rust-toml-parser-1.0.3
                                  rust-toml-writer-1.0.3
                                  rust-tracing-0.1.41
                                  rust-tracing-core-0.1.34
                                  rust-trash-5.2.3
                                  rust-ttf-parser-0.25.1
                                  rust-umask-2.1.0
                                  rust-unicode-bidi-0.3.18
                                  rust-unicode-bidi-mirroring-0.4.0
                                  rust-unicode-ccc-0.4.0
                                  rust-unicode-ident-1.0.19
                                  rust-unicode-normalization-0.1.24
                                  rust-unicode-properties-0.1.3
                                  rust-unicode-script-0.5.7
                                  rust-unicode-segmentation-1.12.0
                                  rust-unicode-vo-0.1.0
                                  rust-unicode-width-0.1.14
                                  rust-unicode-width-0.2.1
                                  rust-url-2.5.7
                                  rust-urlencoding-2.1.3
                                  rust-usvg-0.45.1
                                  rust-utf8-iter-1.0.4
                                  rust-utf8parse-0.2.2
                                  rust-uzers-0.12.1
                                  rust-v-frame-0.3.9
                                  rust-vcpkg-0.2.15
                                  rust-version-compare-0.2.0
                                  rust-version-check-0.9.5
                                  rust-vte-0.15.0
                                  rust-walkdir-2.5.0
                                  rust-wasi-0.11.1+wasi-snapshot-preview1
                                  rust-wasi-0.14.7+wasi-0.2.4
                                  rust-wasip2-1.0.1+wasi-0.2.4
                                  rust-wasm-bindgen-0.2.104
                                  rust-wasm-bindgen-backend-0.2.104
                                  rust-wasm-bindgen-macro-0.2.104
                                  rust-wasm-bindgen-macro-support-0.2.104
                                  rust-wasm-bindgen-shared-0.2.104
                                  rust-weezl-0.1.10
                                  rust-which-4.4.2
                                  rust-winapi-0.3.9
                                  rust-winapi-i686-pc-windows-gnu-0.4.0
                                  rust-winapi-util-0.1.11
                                  rust-winapi-wsapoll-0.1.2
                                  rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                  rust-windows-0.56.0
                                  rust-windows-core-0.56.0
                                  rust-windows-core-0.62.1
                                  rust-windows-implement-0.56.0
                                  rust-windows-implement-0.60.1
                                  rust-windows-interface-0.56.0
                                  rust-windows-interface-0.59.2
                                  rust-windows-link-0.2.0
                                  rust-windows-result-0.1.2
                                  rust-windows-result-0.4.0
                                  rust-windows-strings-0.5.0
                                  rust-windows-sys-0.48.0
                                  rust-windows-sys-0.59.0
                                  rust-windows-sys-0.60.2
                                  rust-windows-sys-0.61.1
                                  rust-windows-targets-0.48.5
                                  rust-windows-targets-0.52.6
                                  rust-windows-targets-0.53.4
                                  rust-windows-aarch64-gnullvm-0.48.5
                                  rust-windows-aarch64-gnullvm-0.52.6
                                  rust-windows-aarch64-gnullvm-0.53.0
                                  rust-windows-aarch64-msvc-0.48.5
                                  rust-windows-aarch64-msvc-0.52.6
                                  rust-windows-aarch64-msvc-0.53.0
                                  rust-windows-i686-gnu-0.48.5
                                  rust-windows-i686-gnu-0.52.6
                                  rust-windows-i686-gnu-0.53.0
                                  rust-windows-i686-gnullvm-0.52.6
                                  rust-windows-i686-gnullvm-0.53.0
                                  rust-windows-i686-msvc-0.48.5
                                  rust-windows-i686-msvc-0.52.6
                                  rust-windows-i686-msvc-0.53.0
                                  rust-windows-x86-64-gnu-0.48.5
                                  rust-windows-x86-64-gnu-0.52.6
                                  rust-windows-x86-64-gnu-0.53.0
                                  rust-windows-x86-64-gnullvm-0.48.5
                                  rust-windows-x86-64-gnullvm-0.52.6
                                  rust-windows-x86-64-gnullvm-0.53.0
                                  rust-windows-x86-64-msvc-0.48.5
                                  rust-windows-x86-64-msvc-0.52.6
                                  rust-windows-x86-64-msvc-0.53.0
                                  rust-winnow-0.7.13
                                  rust-wit-bindgen-0.46.0
                                  rust-writeable-0.6.1
                                  rust-x11-clipboard-0.8.1
                                  rust-x11rb-0.12.0
                                  rust-x11rb-protocol-0.12.0
                                  rust-xmlwriter-0.1.0
                                  rust-xterm-query-0.5.2
                                  rust-yaml-rust-0.4.5
                                  rust-yoke-0.8.0
                                  rust-yoke-derive-0.8.0
                                  rust-zerocopy-0.8.27
                                  rust-zerocopy-derive-0.8.27
                                  rust-zerofrom-0.1.6
                                  rust-zerofrom-derive-0.1.6
                                  rust-zerotrie-0.2.2
                                  rust-zerovec-0.11.4
                                  rust-zerovec-derive-0.11.1
                                  rust-zune-bmp-0.4.0
                                  rust-zune-core-0.4.12
                                  rust-zune-farbfeld-0.4.0
                                  rust-zune-hdr-0.4.0
                                  rust-zune-image-0.4.15
                                  rust-zune-inflate-0.2.54
                                  rust-zune-jpeg-0.4.21
                                  rust-zune-jpegxl-0.4.0
                                  rust-zune-png-0.4.10
                                  rust-zune-ppm-0.4.0
                                  rust-zune-psd-0.4.0
                                  rust-zune-qoi-0.4.10))
                     (himalaya =>
                               (list rust-inflector-0.11.4
                                rust-abnf-core-0.6.0
                                rust-addr2line-0.21.0
                                rust-adler-1.0.2
                                rust-adler2-2.0.0
                                rust-aes-0.8.4
                                rust-ahash-0.8.11
                                rust-aho-corasick-1.1.3
                                rust-aliasable-0.1.3
                                rust-allocator-api2-0.2.21
                                rust-android-tzdata-0.1.1
                                rust-android-system-properties-0.1.5
                                rust-anstream-0.6.18
                                rust-anstyle-1.0.10
                                rust-anstyle-parse-0.2.6
                                rust-anstyle-query-1.1.2
                                rust-anstyle-wincon-3.0.6
                                rust-ariadne-0.2.0
                                rust-async-broadcast-0.7.2
                                rust-async-channel-2.3.1
                                rust-async-executor-1.13.1
                                rust-async-fs-2.1.2
                                rust-async-io-2.4.0
                                rust-async-lock-3.4.0
                                rust-async-process-2.3.0
                                rust-async-recursion-1.1.1
                                rust-async-signal-0.2.10
                                rust-async-task-4.7.1
                                rust-async-trait-0.1.85
                                rust-atomic-waker-1.1.2
                                rust-autocfg-1.4.0
                                rust-backtrace-0.3.71
                                rust-base16ct-0.2.0
                                rust-base64-0.13.1
                                rust-base64-0.21.7
                                rust-base64-0.22.1
                                rust-base64ct-1.6.0
                                rust-bitfield-0.14.0
                                rust-bitflags-1.3.2
                                rust-bitflags-2.7.0
                                rust-block-buffer-0.10.4
                                rust-block-padding-0.3.3
                                rust-blocking-1.6.1
                                rust-blowfish-0.9.1
                                rust-bounded-static-0.8.0
                                rust-bounded-static-derive-0.8.0
                                rust-bstr-1.11.3
                                rust-buffer-redux-1.0.2
                                rust-build-rs-0.1.2
                                rust-bumpalo-3.16.0
                                rust-byteorder-1.5.0
                                rust-bytes-1.9.0
                                rust-camellia-0.1.0
                                rust-cast5-0.11.1
                                rust-cbc-0.1.2
                                rust-cc-1.2.8
                                rust-cesu8-1.1.0
                                rust-cfb-mode-0.8.2
                                rust-cfg-expr-0.15.8
                                rust-cfg-if-1.0.0
                                rust-cfg-aliases-0.2.1
                                rust-chrono-0.4.39
                                rust-chumsky-1.0.0-alpha.7
                                rust-cipher-0.4.4
                                rust-clap-4.5.26
                                rust-clap-builder-4.5.26
                                rust-clap-complete-4.5.42
                                rust-clap-derive-4.5.24
                                rust-clap-lex-0.7.4
                                rust-clap-mangen-0.2.26
                                rust-color-eyre-0.6.3
                                rust-color-spantrace-0.2.1
                                rust-colorchoice-1.0.3
                                rust-combine-4.6.7
                                rust-comfy-table-7.1.3
                                rust-concurrent-queue-2.5.0
                                rust-const-oid-0.9.6
                                rust-conv-0.3.3
                                rust-core-foundation-0.9.4
                                rust-core-foundation-0.10.0
                                rust-core-foundation-sys-0.8.7
                                rust-cpufeatures-0.2.16
                                rust-crc24-0.1.6
                                rust-crc32fast-1.4.2
                                rust-crossbeam-deque-0.8.6
                                rust-crossbeam-epoch-0.9.18
                                rust-crossbeam-utils-0.8.21
                                rust-crossterm-0.25.0
                                rust-crossterm-0.27.0
                                rust-crossterm-0.28.1
                                rust-crossterm-winapi-0.9.1
                                rust-crypto-bigint-0.5.5
                                rust-crypto-common-0.1.6
                                rust-cstr-argument-0.1.2
                                rust-curve25519-dalek-4.1.3
                                rust-curve25519-dalek-derive-0.1.1
                                rust-custom-derive-0.1.7
                                rust-darling-0.10.2
                                rust-darling-0.14.4
                                rust-darling-core-0.10.2
                                rust-darling-core-0.14.4
                                rust-darling-macro-0.10.2
                                rust-darling-macro-0.14.4
                                rust-data-encoding-2.6.0
                                rust-dbus-0.9.7
                                rust-dbus-secret-service-4.0.3
                                rust-der-0.7.9
                                rust-derive-builder-0.12.0
                                rust-derive-builder-core-0.12.0
                                rust-derive-builder-macro-0.12.0
                                rust-des-0.8.1
                                rust-digest-0.10.7
                                rust-dirs-4.0.0
                                rust-dirs-5.0.1
                                rust-dirs-sys-0.3.7
                                rust-dirs-sys-0.4.1
                                rust-displaydoc-0.2.5
                                rust-dyn-clone-1.0.17
                                rust-ecdsa-0.16.9
                                rust-ed25519-2.2.3
                                rust-ed25519-dalek-2.1.1
                                rust-either-1.13.0
                                rust-elliptic-curve-0.13.8
                                rust-email-lib-0.26.4
                                rust-email-macros-0.0.2
                                rust-email-address-0.2.9
                                rust-encoding-rs-0.8.35
                                rust-endi-1.1.0
                                rust-enum-as-inner-0.6.1
                                rust-enumflags2-0.7.10
                                rust-enumflags2-derive-0.7.10
                                rust-equivalent-1.0.1
                                rust-erased-serde-0.4.5
                                rust-errno-0.3.10
                                rust-event-listener-5.4.0
                                rust-event-listener-strategy-0.5.3
                                rust-eyre-0.6.12
                                rust-fastrand-2.3.0
                                rust-ff-0.13.0
                                rust-fiat-crypto-0.2.9
                                rust-filetime-0.2.25
                                rust-fixedbitset-0.4.2
                                rust-flate2-1.0.35
                                rust-fnv-1.0.7
                                rust-form-urlencoded-1.2.1
                                rust-from-variants-0.6.0
                                rust-from-variants-impl-0.6.0
                                rust-fs2-0.4.3
                                rust-futures-0.3.31
                                rust-futures-channel-0.3.31
                                rust-futures-core-0.3.31
                                rust-futures-executor-0.3.31
                                rust-futures-io-0.3.31
                                rust-futures-lite-2.5.0
                                rust-futures-macro-0.3.31
                                rust-futures-sink-0.3.31
                                rust-futures-task-0.3.31
                                rust-futures-util-0.3.31
                                rust-fuzzy-matcher-0.3.7
                                rust-fxhash-0.2.1
                                rust-generic-array-0.14.7
                                rust-gethostname-0.4.3
                                rust-getrandom-0.2.15
                                rust-gimli-0.28.1
                                rust-git2-0.19.0
                                rust-gpg-error-0.6.2
                                rust-gpgme-0.11.0
                                rust-gpgme-sys-0.11.0
                                rust-group-0.13.0
                                rust-hashbrown-0.14.5
                                rust-hashbrown-0.15.2
                                rust-heck-0.5.0
                                rust-hermit-abi-0.4.0
                                rust-hex-0.4.3
                                rust-hickory-proto-0.24.2
                                rust-hickory-resolver-0.24.2
                                rust-hkdf-0.12.4
                                rust-hmac-0.12.1
                                rust-hostname-0.3.1
                                rust-http-1.2.0
                                rust-http-lib-0.1.0
                                rust-httparse-1.9.5
                                rust-iana-time-zone-0.1.61
                                rust-iana-time-zone-haiku-0.1.2
                                rust-icu-collections-1.5.0
                                rust-icu-locid-1.5.0
                                rust-icu-locid-transform-1.5.0
                                rust-icu-locid-transform-data-1.5.0
                                rust-icu-normalizer-1.5.0
                                rust-icu-normalizer-data-1.5.0
                                rust-icu-properties-1.5.1
                                rust-icu-properties-data-1.5.0
                                rust-icu-provider-1.5.0
                                rust-icu-provider-macros-1.5.0
                                rust-idea-0.5.1
                                rust-ident-case-1.0.1
                                rust-idna-1.0.3
                                rust-idna-adapter-1.2.0
                                rust-imap-client-0.2.3
                                rust-imap-codec-2.0.0-alpha.5
                                rust-imap-next-0.3.1
                                rust-imap-types-2.0.0-alpha.4
                                rust-indenter-0.3.3
                                rust-indexmap-2.7.0
                                rust-inotify-0.9.6
                                rust-inotify-sys-0.1.5
                                rust-inout-0.1.3
                                rust-inquire-0.7.5
                                rust-instant-0.1.13
                                rust-ipconfig-0.3.2
                                rust-ipnet-2.10.1
                                rust-is-docker-0.2.0
                                rust-is-wsl-0.4.0
                                rust-is-terminal-polyfill-1.70.1
                                rust-itoa-1.0.14
                                rust-jni-0.19.0
                                rust-jni-sys-0.3.0
                                rust-jobserver-0.1.32
                                rust-js-sys-0.3.76
                                rust-keccak-0.1.5
                                rust-keyring-3.6.1
                                rust-keyring-lib-1.0.2
                                rust-kqueue-1.0.8
                                rust-kqueue-sys-1.0.4
                                rust-lazy-static-1.5.0
                                rust-libc-0.2.169
                                rust-libdbus-sys-0.2.5
                                rust-libgit2-sys-0.17.0+1.8.1
                                rust-libgpg-error-sys-0.6.2
                                rust-libm-0.2.11
                                rust-libredox-0.1.3
                                rust-libz-sys-1.1.21
                                rust-linked-hash-map-0.5.6
                                rust-linux-keyutils-0.2.4
                                rust-linux-raw-sys-0.4.15
                                rust-litemap-0.7.4
                                rust-lock-api-0.4.12
                                rust-log-0.4.24
                                rust-lru-cache-0.1.2
                                rust-mail-builder-0.3.2
                                rust-mail-parser-0.9.4
                                rust-mail-send-0.4.9
                                rust-maildirs-0.2.2
                                rust-match-cfg-0.1.0
                                rust-matchers-0.1.0
                                rust-md-5-0.10.6
                                rust-md5-0.7.0
                                rust-memchr-2.7.4
                                rust-memoffset-0.7.1
                                rust-memoffset-0.9.1
                                rust-mime-0.3.17
                                rust-mime-guess-2.0.5
                                rust-minimal-lexical-0.2.1
                                rust-miniz-oxide-0.7.4
                                rust-miniz-oxide-0.8.2
                                rust-mio-0.8.11
                                rust-mio-1.0.3
                                rust-mml-lib-1.1.1
                                rust-nanohtml2text-0.1.5
                                rust-newline-converter-0.3.0
                                rust-nix-0.29.0
                                rust-nom-7.1.3
                                rust-notify-6.1.1
                                rust-notmuch-0.8.0
                                rust-nu-ansi-term-0.46.0
                                rust-num-0.4.3
                                rust-num-bigint-0.4.6
                                rust-num-bigint-dig-0.8.4
                                rust-num-complex-0.4.6
                                rust-num-derive-0.4.2
                                rust-num-integer-0.1.46
                                rust-num-iter-0.1.45
                                rust-num-rational-0.4.2
                                rust-num-traits-0.2.19
                                rust-oauth-lib-2.0.0
                                rust-oauth2-5.0.0-rc.1
                                rust-object-0.32.2
                                rust-once-cell-1.20.2
                                rust-open-5.3.2
                                rust-openssl-probe-0.1.5
                                rust-option-ext-0.2.0
                                rust-ordered-stream-0.2.0
                                rust-os-str-bytes-6.6.1
                                rust-ouroboros-0.15.6
                                rust-ouroboros-macro-0.15.6
                                rust-overload-0.1.1
                                rust-owo-colors-3.5.0
                                rust-p256-0.13.2
                                rust-p384-0.13.0
                                rust-parking-2.2.1
                                rust-parking-lot-0.11.2
                                rust-parking-lot-0.12.3
                                rust-parking-lot-core-0.8.6
                                rust-parking-lot-core-0.9.10
                                rust-paste-1.0.15
                                rust-pathdiff-0.2.3
                                rust-pem-rfc7468-0.7.0
                                rust-percent-encoding-2.3.1
                                rust-petgraph-0.6.5
                                rust-pgp-0.10.2
                                rust-pgp-lib-1.0.0
                                rust-pimalaya-tui-0.2.2
                                rust-pin-project-lite-0.2.16
                                rust-pin-utils-0.1.0
                                rust-piper-0.2.4
                                rust-pkcs1-0.7.5
                                rust-pkcs8-0.10.2
                                rust-pkg-config-0.3.31
                                rust-polling-3.7.4
                                rust-ppv-lite86-0.2.20
                                rust-primeorder-0.13.6
                                rust-proc-macro-crate-3.2.0
                                rust-proc-macro-error-1.0.4
                                rust-proc-macro-error-attr-1.0.4
                                rust-proc-macro2-1.0.93
                                rust-process-lib-1.0.0
                                rust-quick-error-1.2.3
                                rust-quote-1.0.38
                                rust-rand-0.8.5
                                rust-rand-chacha-0.3.1
                                rust-rand-core-0.6.4
                                rust-rayon-1.10.0
                                rust-rayon-core-1.12.1
                                rust-redox-syscall-0.2.16
                                rust-redox-syscall-0.5.8
                                rust-redox-users-0.4.6
                                rust-regex-1.11.1
                                rust-regex-automata-0.1.10
                                rust-regex-automata-0.3.9
                                rust-regex-automata-0.4.9
                                rust-regex-syntax-0.6.29
                                rust-regex-syntax-0.7.5
                                rust-regex-syntax-0.8.5
                                rust-resolv-conf-0.7.0
                                rust-rfc6979-0.4.0
                                rust-ring-0.17.8
                                rust-rip-starttls-0.1.0
                                rust-ripemd-0.1.3
                                rust-roff-0.2.2
                                rust-rsa-0.9.7
                                rust-rustc-demangle-0.1.24
                                rust-rustc-version-0.4.1
                                rust-rustix-0.38.43
                                rust-rustls-0.21.12
                                rust-rustls-0.23.21
                                rust-rustls-native-certs-0.7.3
                                rust-rustls-pemfile-1.0.4
                                rust-rustls-pemfile-2.2.0
                                rust-rustls-pki-types-1.10.1
                                rust-rustls-platform-verifier-0.3.4
                                rust-rustls-platform-verifier-0.4.0
                                rust-rustls-platform-verifier-android-0.1.1
                                rust-rustls-webpki-0.101.7
                                rust-rustls-webpki-0.102.8
                                rust-rustversion-1.0.19
                                rust-ryu-1.0.18
                                rust-same-file-1.0.6
                                rust-schannel-0.1.27
                                rust-scopeguard-1.2.0
                                rust-sct-0.7.1
                                rust-sec1-0.7.3
                                rust-secret-lib-1.0.0
                                rust-secret-service-4.0.0
                                rust-security-framework-2.11.1
                                rust-security-framework-3.2.0
                                rust-security-framework-sys-2.14.0
                                rust-semver-1.0.24
                                rust-serde-1.0.217
                                rust-serde-toml-merge-0.3.8
                                rust-serde-xml-rs-0.6.0
                                rust-serde-derive-1.0.217
                                rust-serde-fmt-1.0.3
                                rust-serde-json-1.0.135
                                rust-serde-path-to-error-0.1.16
                                rust-serde-repr-0.1.19
                                rust-serde-spanned-0.6.8
                                rust-sha1-0.10.6
                                rust-sha2-0.10.8
                                rust-sha3-0.10.8
                                rust-sharded-slab-0.1.7
                                rust-shellexpand-3.1.0
                                rust-shellexpand-utils-0.2.1
                                rust-shlex-1.3.0
                                rust-signal-hook-0.3.17
                                rust-signal-hook-mio-0.2.4
                                rust-signal-hook-registry-1.4.2
                                rust-signature-2.2.0
                                rust-slab-0.4.9
                                rust-sled-0.34.7
                                rust-smallvec-1.13.2
                                rust-smtp-proto-0.1.5
                                rust-socket2-0.5.8
                                rust-spin-0.9.8
                                rust-spki-0.7.3
                                rust-stable-deref-trait-1.2.0
                                rust-static-assertions-1.1.0
                                rust-strsim-0.9.3
                                rust-strsim-0.10.0
                                rust-strsim-0.11.1
                                rust-strum-0.26.3
                                rust-strum-macros-0.26.4
                                rust-subtle-2.6.1
                                rust-sval-2.13.2
                                rust-sval-buffer-2.13.2
                                rust-sval-dynamic-2.13.2
                                rust-sval-fmt-2.13.2
                                rust-sval-json-2.13.2
                                rust-sval-nested-2.13.2
                                rust-sval-ref-2.13.2
                                rust-sval-serde-2.13.2
                                rust-syn-1.0.109
                                rust-syn-2.0.96
                                rust-synstructure-0.13.1
                                rust-system-deps-6.2.2
                                rust-target-lexicon-0.12.16
                                rust-tempfile-3.15.0
                                rust-terminal-size-0.4.1
                                rust-thiserror-1.0.69
                                rust-thiserror-2.0.11
                                rust-thiserror-impl-1.0.69
                                rust-thiserror-impl-2.0.11
                                rust-thread-local-1.1.8
                                rust-tinystr-0.7.6
                                rust-tinyvec-1.8.1
                                rust-tinyvec-macros-0.1.1
                                rust-tokio-1.43.0
                                rust-tokio-macros-2.5.0
                                rust-tokio-rustls-0.24.1
                                rust-tokio-rustls-0.26.1
                                rust-toml-0.8.19
                                rust-toml-datetime-0.6.8
                                rust-toml-edit-0.22.22
                                rust-tracing-0.1.41
                                rust-tracing-attributes-0.1.28
                                rust-tracing-core-0.1.33
                                rust-tracing-error-0.2.1
                                rust-tracing-log-0.2.0
                                rust-tracing-subscriber-0.3.19
                                rust-tree-magic-mini-3.1.6
                                rust-twofish-0.7.1
                                rust-typeid-1.0.2
                                rust-typenum-1.17.0
                                rust-uds-windows-1.1.0
                                rust-unicase-2.8.1
                                rust-unicode-ident-1.0.14
                                rust-unicode-segmentation-1.12.0
                                rust-unicode-width-0.1.14
                                rust-unicode-width-0.2.0
                                rust-untrusted-0.9.0
                                rust-ureq-3.0.0-rc5
                                rust-ureq-proto-0.2.3
                                rust-url-2.5.4
                                rust-urlencoding-2.1.3
                                rust-utf-8-0.7.6
                                rust-utf16-iter-1.0.5
                                rust-utf7-imap-0.3.2
                                rust-utf8-iter-1.0.4
                                rust-utf8parse-0.2.2
                                rust-uuid-0.8.2
                                rust-uuid-1.11.1
                                rust-valuable-0.1.0
                                rust-value-bag-1.10.0
                                rust-value-bag-serde1-1.10.0
                                rust-value-bag-sval2-1.10.0
                                rust-vcpkg-0.2.15
                                rust-version-compare-0.2.0
                                rust-version-check-0.9.5
                                rust-walkdir-2.5.0
                                rust-wasi-0.11.0+wasi-snapshot-preview1
                                rust-wasm-bindgen-0.2.99
                                rust-wasm-bindgen-backend-0.2.99
                                rust-wasm-bindgen-macro-0.2.99
                                rust-wasm-bindgen-macro-support-0.2.99
                                rust-wasm-bindgen-shared-0.2.99
                                rust-webpki-root-certs-0.26.7
                                rust-webpki-roots-0.26.7
                                rust-widestring-1.1.0
                                rust-winapi-0.3.9
                                rust-winapi-i686-pc-windows-gnu-0.4.0
                                rust-winapi-util-0.1.9
                                rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                rust-windows-core-0.52.0
                                rust-windows-sys-0.48.0
                                rust-windows-sys-0.52.0
                                rust-windows-sys-0.59.0
                                rust-windows-targets-0.48.5
                                rust-windows-targets-0.52.6
                                rust-windows-aarch64-gnullvm-0.48.5
                                rust-windows-aarch64-gnullvm-0.52.6
                                rust-windows-aarch64-msvc-0.48.5
                                rust-windows-aarch64-msvc-0.52.6
                                rust-windows-i686-gnu-0.48.5
                                rust-windows-i686-gnu-0.52.6
                                rust-windows-i686-gnullvm-0.52.6
                                rust-windows-i686-msvc-0.48.5
                                rust-windows-i686-msvc-0.52.6
                                rust-windows-x86-64-gnu-0.48.5
                                rust-windows-x86-64-gnu-0.52.6
                                rust-windows-x86-64-gnullvm-0.48.5
                                rust-windows-x86-64-gnullvm-0.52.6
                                rust-windows-x86-64-msvc-0.48.5
                                rust-windows-x86-64-msvc-0.52.6
                                rust-winnow-0.6.24
                                rust-winreg-0.10.1
                                rust-winreg-0.50.0
                                rust-winreg-0.52.0
                                rust-write16-1.0.0
                                rust-writeable-0.5.5
                                rust-x25519-dalek-2.0.1
                                rust-xdg-home-1.3.0
                                rust-xml-rs-0.8.25
                                rust-yansi-0.5.1
                                rust-yoke-0.7.5
                                rust-yoke-derive-0.7.5
                                rust-z-base-32-0.1.4
                                rust-zbus-4.4.0
                                rust-zbus-macros-4.4.0
                                rust-zbus-names-3.0.0
                                rust-zerocopy-0.7.35
                                rust-zerocopy-derive-0.7.35
                                rust-zerofrom-0.1.5
                                rust-zerofrom-derive-0.1.5
                                rust-zeroize-1.8.1
                                rust-zeroize-derive-1.4.2
                                rust-zerovec-0.10.4
                                rust-zerovec-derive-0.10.3
                                rust-zvariant-4.2.0
                                rust-zvariant-derive-4.2.0
                                rust-zvariant-utils-2.1.0))
                     (overskride =>
                                 (list rust-addr2line-0.21.0
                                  rust-adler-1.0.2
                                  rust-aho-corasick-1.1.2
                                  rust-anyhow-1.0.79
                                  rust-autocfg-1.1.0
                                  rust-backtrace-0.3.69
                                  rust-bitflags-1.3.2
                                  rust-bitflags-2.4.2
                                  rust-block-0.1.6
                                  rust-bluer-0.16.1
                                  rust-bytes-1.5.0
                                  rust-cairo-rs-0.18.5
                                  rust-cairo-sys-rs-0.18.2
                                  rust-cc-1.0.83
                                  rust-cfg-expr-0.15.7
                                  rust-cfg-if-1.0.0
                                  rust-custom-debug-0.5.1
                                  rust-custom-debug-derive-0.5.1
                                  rust-dbus-0.9.7
                                  rust-dbus-crossroads-0.5.2
                                  rust-dbus-tokio-0.7.6
                                  rust-displaydoc-0.2.4
                                  rust-equivalent-1.0.1
                                  rust-field-offset-0.3.6
                                  rust-futures-0.3.30
                                  rust-futures-channel-0.3.30
                                  rust-futures-core-0.3.30
                                  rust-futures-executor-0.3.30
                                  rust-futures-io-0.3.30
                                  rust-futures-macro-0.3.30
                                  rust-futures-sink-0.3.30
                                  rust-futures-task-0.3.30
                                  rust-futures-util-0.3.30
                                  rust-gdk-pixbuf-0.18.5
                                  rust-gdk-pixbuf-sys-0.18.0
                                  rust-gdk4-0.7.3
                                  rust-gdk4-sys-0.7.2
                                  rust-getrandom-0.2.12
                                  rust-gettext-rs-0.7.0
                                  rust-gettext-sys-0.21.3
                                  rust-gimli-0.28.1
                                  rust-gio-0.18.4
                                  rust-gio-sys-0.18.1
                                  rust-glib-0.18.5
                                  rust-glib-macros-0.18.5
                                  rust-glib-sys-0.18.1
                                  rust-gobject-sys-0.18.0
                                  rust-graphene-rs-0.18.1
                                  rust-graphene-sys-0.18.1
                                  rust-gsk4-0.7.3
                                  rust-gsk4-sys-0.7.3
                                  rust-gtk4-0.7.3
                                  rust-gtk4-macros-0.7.2
                                  rust-gtk4-sys-0.7.3
                                  rust-hashbrown-0.14.3
                                  rust-heck-0.4.1
                                  rust-hermit-abi-0.3.5
                                  rust-hex-0.4.3
                                  rust-indexmap-2.2.2
                                  rust-itoa-1.0.10
                                  rust-lazy-static-1.4.0
                                  rust-libadwaita-0.5.3
                                  rust-libadwaita-sys-0.5.3
                                  rust-libc-0.2.153
                                  rust-libdbus-sys-0.2.5
                                  rust-libpulse-binding-2.28.1
                                  rust-libpulse-sys-1.21.0
                                  rust-locale-config-0.3.0
                                  rust-lock-api-0.4.11
                                  rust-log-0.4.20
                                  rust-macaddr-1.0.1
                                  rust-malloc-buf-0.0.6
                                  rust-memchr-2.7.1
                                  rust-memoffset-0.9.0
                                  rust-miniz-oxide-0.7.2
                                  rust-mio-0.8.10
                                  rust-nix-0.27.1
                                  rust-num-derive-0.3.3
                                  rust-num-derive-0.4.2
                                  rust-num-traits-0.2.18
                                  rust-num-cpus-1.16.0
                                  rust-objc-0.2.7
                                  rust-objc-foundation-0.1.1
                                  rust-objc-id-0.1.1
                                  rust-object-0.32.2
                                  rust-once-cell-1.19.0
                                  rust-pango-0.18.3
                                  rust-pango-sys-0.18.0
                                  rust-parking-lot-0.12.1
                                  rust-parking-lot-core-0.9.9
                                  rust-phf-0.11.2
                                  rust-phf-generator-0.11.2
                                  rust-phf-macros-0.11.2
                                  rust-phf-shared-0.11.2
                                  rust-pin-project-1.1.4
                                  rust-pin-project-internal-1.1.4
                                  rust-pin-project-lite-0.2.13
                                  rust-pin-utils-0.1.0
                                  rust-pkg-config-0.3.29
                                  rust-proc-macro-crate-1.3.1
                                  rust-proc-macro-crate-2.0.2
                                  rust-proc-macro-error-1.0.4
                                  rust-proc-macro-error-attr-1.0.4
                                  rust-proc-macro2-1.0.78
                                  rust-quote-1.0.35
                                  rust-rand-0.8.5
                                  rust-rand-core-0.6.4
                                  rust-redox-syscall-0.4.1
                                  rust-regex-1.10.3
                                  rust-regex-automata-0.4.5
                                  rust-regex-syntax-0.8.2
                                  rust-rustc-demangle-0.1.23
                                  rust-rustc-version-0.4.0
                                  rust-rustversion-1.0.14
                                  rust-ryu-1.0.16
                                  rust-scopeguard-1.2.0
                                  rust-semver-1.0.21
                                  rust-serde-1.0.196
                                  rust-serde-derive-1.0.196
                                  rust-serde-json-1.0.113
                                  rust-serde-spanned-0.6.5
                                  rust-signal-hook-registry-1.4.1
                                  rust-siphasher-0.3.11
                                  rust-slab-0.4.9
                                  rust-smallvec-1.13.1
                                  rust-socket2-0.5.5
                                  rust-strum-0.25.0
                                  rust-strum-macros-0.25.3
                                  rust-syn-1.0.109
                                  rust-syn-2.0.48
                                  rust-synstructure-0.12.6
                                  rust-system-deps-6.2.0
                                  rust-target-lexicon-0.12.13
                                  rust-temp-dir-0.1.12
                                  rust-thiserror-1.0.56
                                  rust-thiserror-impl-1.0.56
                                  rust-tokio-1.36.0
                                  rust-tokio-macros-2.2.0
                                  rust-tokio-stream-0.1.14
                                  rust-tokio-util-0.7.10
                                  rust-toml-0.8.2
                                  rust-toml-datetime-0.6.3
                                  rust-toml-edit-0.19.15
                                  rust-toml-edit-0.20.2
                                  rust-unicode-ident-1.0.12
                                  rust-unicode-xid-0.2.4
                                  rust-uuid-1.7.0
                                  rust-version-compare-0.1.1
                                  rust-version-check-0.9.4
                                  rust-wasi-0.11.0+wasi-snapshot-preview1
                                  rust-winapi-0.3.9
                                  rust-winapi-i686-pc-windows-gnu-0.4.0
                                  rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                  rust-windows-sys-0.48.0
                                  rust-windows-targets-0.48.5
                                  rust-windows-aarch64-gnullvm-0.48.5
                                  rust-windows-aarch64-msvc-0.48.5
                                  rust-windows-i686-gnu-0.48.5
                                  rust-windows-i686-msvc-0.48.5
                                  rust-windows-x86-64-gnu-0.48.5
                                  rust-windows-x86-64-gnullvm-0.48.5
                                  rust-windows-x86-64-msvc-0.48.5
                                  rust-winnow-0.5.39))
                     (pimsync =>
                              (list rust-addr2line-0.21.0
                                    rust-adler-1.0.2
                                    rust-anyhow-1.0.75
                                    rust-async-lock-3.4.0
                                    rust-async-trait-0.1.73
                                    rust-autocfg-1.1.0
                                    rust-backtrace-0.3.69
                                    rust-base64-0.21.3
                                    rust-base64-0.22.1
                                    rust-bitflags-1.3.2
                                    rust-bitflags-2.4.0
                                    rust-block-buffer-0.10.4
                                    rust-bumpalo-3.16.0
                                    rust-bytes-1.4.0
                                    rust-camino-1.1.6
                                    rust-cc-1.2.16
                                    rust-cfg-if-1.0.0
                                    rust-colored-2.1.0
                                    rust-concurrent-queue-2.5.0
                                    rust-core-foundation-0.9.3
                                    rust-core-foundation-sys-0.8.4
                                    rust-cpufeatures-0.2.9
                                    rust-crossbeam-channel-0.5.13
                                    rust-crossbeam-epoch-0.9.18
                                    rust-crossbeam-utils-0.8.20
                                    rust-crypto-common-0.1.6
                                    rust-deranged-0.3.8
                                    rust-digest-0.10.7
                                    rust-domain-0.10.3
                                    rust-errno-0.3.3
                                    rust-errno-dragonfly-0.1.2
                                    rust-event-listener-5.3.1
                                    rust-event-listener-strategy-0.5.2
                                    rust-fastrand-2.0.0
                                    rust-fnv-1.0.7
                                    rust-futures-channel-0.3.28
                                    rust-futures-core-0.3.31
                                    rust-futures-macro-0.3.31
                                    rust-futures-task-0.3.31
                                    rust-futures-util-0.3.31
                                    rust-generic-array-0.14.7
                                    rust-getrandom-0.2.10
                                    rust-gimli-0.28.0
                                    rust-hermit-abi-0.3.9
                                    rust-hex-0.4.3
                                    rust-http-1.1.0
                                    rust-http-body-1.0.1
                                    rust-http-body-util-0.1.2
                                    rust-httparse-1.8.0
                                    rust-httpdate-1.0.3
                                    rust-hyper-1.4.1
                                    rust-hyper-rustls-0.26.0
                                    rust-hyper-util-0.1.7
                                    rust-hyperlocal-0.9.1
                                    rust-inotify-0.11.0
                                    rust-inotify-sys-0.1.5
                                    rust-itoa-1.0.9
                                    rust-js-sys-0.3.72
                                    rust-lazy-static-1.4.0
                                    rust-lexopt-0.3.0
                                    rust-libc-0.2.161
                                    rust-libdav-0.9.1
                                    rust-linux-raw-sys-0.4.5
                                    rust-lock-api-0.4.10
                                    rust-log-0.4.20
                                    rust-memchr-2.6.2
                                    rust-mime-0.3.17
                                    rust-miniz-oxide-0.7.1
                                    rust-mio-1.0.2
                                    rust-moka-0.12.8
                                    rust-object-0.32.0
                                    rust-octseq-0.5.2
                                    rust-once-cell-1.18.0
                                    rust-openssl-probe-0.1.5
                                    rust-parking-2.2.1
                                    rust-parking-lot-0.12.1
                                    rust-parking-lot-core-0.9.8
                                    rust-pin-project-1.1.5
                                    rust-pin-project-internal-1.1.5
                                    rust-pin-project-lite-0.2.13
                                    rust-pin-utils-0.1.0
                                    rust-pkg-config-0.3.29
                                    rust-ppv-lite86-0.2.17
                                    rust-proc-macro2-1.0.86
                                    rust-quanta-0.12.3
                                    rust-quote-1.0.36
                                    rust-rand-0.8.5
                                    rust-rand-chacha-0.3.1
                                    rust-rand-core-0.6.4
                                    rust-raw-cpuid-11.2.0
                                    rust-redox-syscall-0.3.5
                                    rust-ring-0.17.14
                                    rust-roxmltree-0.20.0
                                    rust-rustc-demangle-0.1.23
                                    rust-rustc-version-0.4.1
                                    rust-rustix-0.38.10
                                    rust-rustls-0.22.4
                                    rust-rustls-native-certs-0.7.0
                                    rust-rustls-pemfile-2.0.0
                                    rust-rustls-pki-types-1.7.0
                                    rust-rustls-webpki-0.102.4
                                    rust-scfg-0.3.1
                                    rust-schannel-0.1.22
                                    rust-scopeguard-1.2.0
                                    rust-security-framework-2.9.2
                                    rust-security-framework-sys-2.9.1
                                    rust-semver-1.0.23
                                    rust-serde-1.0.188
                                    rust-serde-derive-1.0.188
                                    rust-sha2-0.10.7
                                    rust-shell-words-1.1.0
                                    rust-shlex-1.3.0
                                    rust-signal-hook-registry-1.4.1
                                    rust-simple-logger-4.2.0
                                    rust-slab-0.4.9
                                    rust-smallvec-1.13.2
                                    rust-socket2-0.5.7
                                    rust-sqlite-0.32.0
                                    rust-sqlite3-src-0.5.1
                                    rust-sqlite3-sys-0.15.2
                                    rust-subtle-2.5.0
                                    rust-syn-2.0.67
                                    rust-tagptr-0.2.0
                                    rust-tempfile-3.8.0
                                    rust-thiserror-1.0.47
                                    rust-thiserror-impl-1.0.47
                                    rust-time-0.3.28
                                    rust-time-core-0.1.1
                                    rust-tokio-1.41.0
                                    rust-tokio-macros-2.4.0
                                    rust-tokio-rustls-0.25.0
                                    rust-tower-0.4.13
                                    rust-tower-0.5.1
                                    rust-tower-http-0.6.2
                                    rust-tower-layer-0.3.3
                                    rust-tower-service-0.3.3
                                    rust-tracing-0.1.40
                                    rust-tracing-attributes-0.1.27
                                    rust-tracing-core-0.1.32
                                    rust-triomphe-0.1.11
                                    rust-try-lock-0.2.4
                                    rust-typenum-1.16.0
                                    rust-unicode-ident-1.0.11
                                    rust-untrusted-0.9.0
                                    rust-uuid-1.11.0
                                    rust-version-check-0.9.4
                                    rust-vparser-1.0.1
                                    rust-want-0.3.1
                                    rust-wasi-0.11.0+wasi-snapshot-preview1
                                    rust-wasm-bindgen-0.2.95
                                    rust-wasm-bindgen-backend-0.2.95
                                    rust-wasm-bindgen-macro-0.2.95
                                    rust-wasm-bindgen-macro-support-0.2.95
                                    rust-wasm-bindgen-shared-0.2.95
                                    rust-web-sys-0.3.72
                                    rust-winapi-0.3.9
                                    rust-winapi-i686-pc-windows-gnu-0.4.0
                                    rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                    rust-windows-sys-0.42.0
                                    rust-windows-sys-0.48.0
                                    rust-windows-sys-0.52.0
                                    rust-windows-targets-0.48.5
                                    rust-windows-targets-0.52.6
                                    rust-windows-aarch64-gnullvm-0.42.2
                                    rust-windows-aarch64-gnullvm-0.48.5
                                    rust-windows-aarch64-gnullvm-0.52.6
                                    rust-windows-aarch64-msvc-0.42.2
                                    rust-windows-aarch64-msvc-0.48.5
                                    rust-windows-aarch64-msvc-0.52.6
                                    rust-windows-i686-gnu-0.42.2
                                    rust-windows-i686-gnu-0.48.5
                                    rust-windows-i686-gnu-0.52.6
                                    rust-windows-i686-gnullvm-0.52.6
                                    rust-windows-i686-msvc-0.42.2
                                    rust-windows-i686-msvc-0.48.5
                                    rust-windows-i686-msvc-0.52.6
                                    rust-windows-x86-64-gnu-0.42.2
                                    rust-windows-x86-64-gnu-0.48.5
                                    rust-windows-x86-64-gnu-0.52.6
                                    rust-windows-x86-64-gnullvm-0.42.2
                                    rust-windows-x86-64-gnullvm-0.48.5
                                    rust-windows-x86-64-gnullvm-0.52.6
                                    rust-windows-x86-64-msvc-0.42.2
                                    rust-windows-x86-64-msvc-0.48.5
                                    rust-windows-x86-64-msvc-0.52.6
                                    rust-zeroize-1.7.0)))
