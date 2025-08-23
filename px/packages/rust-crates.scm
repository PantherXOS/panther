;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages rust-crates)
  #:use-module (guix build-system cargo)
  #:export (lookup-cargo-inputs))


(define rust-addr2line-0.21.0
  (crate-source "addr2line" "0.21.0"
                "1jx0k3iwyqr8klqbzk6kjvr496yd94aspis10vwsj5wy7gib4c4a"))

(define rust-adler-1.0.2
  (crate-source "adler" "1.0.2"
                "1zim79cvzd5yrkzl3nyfx0avijwgk9fqv3yrscdy1cc79ih02qpj"))

(define rust-anyhow-1.0.75
  (crate-source "anyhow" "1.0.75"
                "1rmcjkim91c5mw7h9wn8nv0k6x118yz0xg0z1q18svgn42mqqrm4"))

(define rust-async-lock-3.4.0
  (crate-source "async-lock" "3.4.0"
                "060vh45i809wcqyxzs5g69nqiqah7ydz0hpkcjys9258vqn4fvpz"))

(define rust-async-trait-0.1.73
  (crate-source "async-trait" "0.1.73"
                "1w60x18qm18drm8pdl0ix4jai83nh8hlwfjswca3dh4096rww05w"))

(define rust-autocfg-1.1.0
  (crate-source "autocfg" "1.1.0"
                "1ylp3cb47ylzabimazvbz9ms6ap784zhb6syaz6c1jqpmcmq0s6l"))

(define rust-backtrace-0.3.69
  (crate-source "backtrace" "0.3.69"
                "0dsq23dhw4pfndkx2nsa1ml2g31idm7ss7ljxp8d57avygivg290"))

(define rust-base64-0.21.3
  (crate-source "base64" "0.21.3"
                "0lvf1ishhckkjwiamhqr3iwy5ddrzgvgqfkblwkcaxrxqvxwwka1"))

(define rust-base64-0.22.1
  (crate-source "base64" "0.22.1"
                "1imqzgh7bxcikp5vx3shqvw9j09g9ly0xr0jma0q66i52r7jbcvj"))

(define rust-bitflags-1.3.2
  (crate-source "bitflags" "1.3.2"
                "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))

(define rust-bitflags-2.4.0
  (crate-source "bitflags" "2.4.0"
                "0dc6xa7flfl59makmhixjcrslwlvdxxwrgxbr8p7bkvz53k2ls5l"))

(define rust-block-buffer-0.10.4
  (crate-source "block-buffer" "0.10.4"
                "0w9sa2ypmrsqqvc20nhwr75wbb5cjr4kkyhpjm1z1lv2kdicfy1h"))

(define rust-bumpalo-3.16.0
  (crate-source "bumpalo" "3.16.0"
                "0b015qb4knwanbdlp1x48pkb4pm57b8gidbhhhxr900q2wb6fabr"))

(define rust-bytes-1.4.0
  (crate-source "bytes" "1.4.0"
                "1gkh3fk4fm9xv5znlib723h5md5sxsvbd5113sbxff6g1lmgvcl9"))

(define rust-camino-1.1.6
  (crate-source "camino" "1.1.6"
                "171vzfyrm2jmajd70q1m774297y028kadgm7cfw4kxc8lfsr57n5"))

(define rust-cc-1.2.16
  (crate-source "cc" "1.2.16"
                "131bhgafc1i86vvjipkj0kwzz0hlpwrkl8mdbmzyq2g69calqwdy"))

(define rust-cfg-if-1.0.0
  (crate-source "cfg-if" "1.0.0"
                "1za0vb97n4brpzpv8lsbnzmq5r8f2b0cpqqr0sy8h5bn751xxwds"))

(define rust-colored-2.1.0
  (crate-source "colored" "2.1.0"
                "1f4h9p64snrnz4x432iza15p4diqjcgpmpvhi956d6r1rq61bwnb"))

(define rust-concurrent-queue-2.5.0
  (crate-source "concurrent-queue" "2.5.0"
                "0wrr3mzq2ijdkxwndhf79k952cp4zkz35ray8hvsxl96xrx1k82c"))

(define rust-core-foundation-0.9.3
  (crate-source "core-foundation" "0.9.3"
                "0ii1ihpjb30fk38gdikm5wqlkmyr8k46fh4k2r8sagz5dng7ljhr"))

(define rust-core-foundation-sys-0.8.4
  ;; TODO: Check bundled sources.
  (crate-source "core-foundation-sys" "0.8.4"
                "1yhf471qj6snnm2mcswai47vsbc9w30y4abmdp4crb4av87sb5p4"))

(define rust-cpufeatures-0.2.9
  (crate-source "cpufeatures" "0.2.9"
                "1wg1vmsx3gd30xkc7h7r6nfx7njx063hqjimgyrb0qj17bzpcyx1"))

(define rust-crossbeam-channel-0.5.13
  (crate-source "crossbeam-channel" "0.5.13"
                "1wkx45r34v7g3wyi3lg2wz536lrrrab4h4hh741shfhr8rlhsj1k"))

(define rust-crossbeam-epoch-0.9.18
  (crate-source "crossbeam-epoch" "0.9.18"
                "03j2np8llwf376m3fxqx859mgp9f83hj1w34153c7a9c7i5ar0jv"))

(define rust-crossbeam-utils-0.8.20
  (crate-source "crossbeam-utils" "0.8.20"
                "100fksq5mm1n7zj242cclkw6yf7a4a8ix3lvpfkhxvdhbda9kv12"))

(define rust-crypto-common-0.1.6
  (crate-source "crypto-common" "0.1.6"
                "1cvby95a6xg7kxdz5ln3rl9xh66nz66w46mm3g56ri1z5x815yqv"))

(define rust-deranged-0.3.8
  (crate-source "deranged" "0.3.8"
                "0ikrhil2621rz9haakphdzrx035qwr175f639p8qyrazjj56wsgj"))

(define rust-digest-0.10.7
  (crate-source "digest" "0.10.7"
                "14p2n6ih29x81akj097lvz7wi9b6b9hvls0lwrv7b6xwyy0s5ncy"))

(define rust-domain-0.10.3
  (crate-source "domain" "0.10.3"
                "13f2l5g9890v1ilgn6z16y09p1as7a7ssa6dcf5aidpkv5k8c034"))

(define rust-errno-0.3.3
  (crate-source "errno" "0.3.3"
                "1pfv4gygg742cwi21gw88h4f7q5kvwkpk7b3xxpmrqh8hlc2cr8k"))

(define rust-errno-dragonfly-0.1.2
  (crate-source "errno-dragonfly" "0.1.2"
                "1grrmcm6q8512hkq5yzch3yv8wafflc2apbmsaabiyk44yqz2s5a"))

(define rust-event-listener-5.3.1
  (crate-source "event-listener" "5.3.1"
                "1fkm6q4hjn61wl52xyqyyxai0x9w0ngrzi0wf1qsf8vhsadvwck0"))

(define rust-event-listener-strategy-0.5.2
  (crate-source "event-listener-strategy" "0.5.2"
                "18f5ri227khkayhv3ndv7yl4rnasgwksl2jhwgafcxzr7324s88g"))

(define rust-fastrand-2.0.0
  (crate-source "fastrand" "2.0.0"
                "0r17m5p8ym5pa1f6cp8rix78ggclg6llnw5hxg168cr56wcdr6b9"))

(define rust-fnv-1.0.7
  (crate-source "fnv" "1.0.7"
                "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz"))

(define rust-futures-channel-0.3.28
  (crate-source "futures-channel" "0.3.28"
                "1wmm9wm5zjigxz61qkscmxp7c30zp08dy63spjz5pch9gva1hmcm"))

(define rust-futures-core-0.3.31
  (crate-source "futures-core" "0.3.31"
                "0gk6yrxgi5ihfanm2y431jadrll00n5ifhnpx090c2f2q1cr1wh5"))

(define rust-futures-macro-0.3.31
  (crate-source "futures-macro" "0.3.31"
                "0l1n7kqzwwmgiznn0ywdc5i24z72zvh9q1dwps54mimppi7f6bhn"))

(define rust-futures-task-0.3.31
  (crate-source "futures-task" "0.3.31"
                "124rv4n90f5xwfsm9qw6y99755y021cmi5dhzh253s920z77s3zr"))

(define rust-futures-util-0.3.31
  (crate-source "futures-util" "0.3.31"
                "10aa1ar8bgkgbr4wzxlidkqkcxf77gffyj8j7768h831pcaq784z"))

(define rust-generic-array-0.14.7
  (crate-source "generic-array" "0.14.7"
                "16lyyrzrljfq424c3n8kfwkqihlimmsg5nhshbbp48np3yjrqr45"))

(define rust-getrandom-0.2.10
  (crate-source "getrandom" "0.2.10"
                "09zlimhhskzf7cmgcszix05wyz2i6fcpvh711cv1klsxl6r3chdy"))

(define rust-gimli-0.28.0
  (crate-source "gimli" "0.28.0"
                "1h7hcl3chfvd2gfrrxjymnwj7anqxjslvz20kcargkvsya2dgf3g"))

(define rust-hermit-abi-0.3.9
  (crate-source "hermit-abi" "0.3.9"
                "092hxjbjnq5fmz66grd9plxd0sh6ssg5fhgwwwqbrzgzkjwdycfj"))

(define rust-hex-0.4.3
  (crate-source "hex" "0.4.3"
                "0w1a4davm1lgzpamwnba907aysmlrnygbqmfis2mqjx5m552a93z"))

(define rust-http-1.1.0
  (crate-source "http" "1.1.0"
                "0n426lmcxas6h75c2cp25m933pswlrfjz10v91vc62vib2sdvf91"))

(define rust-http-body-1.0.1
  (crate-source "http-body" "1.0.1"
                "111ir5k2b9ihz5nr9cz7cwm7fnydca7dx4hc7vr16scfzghxrzhy"))

(define rust-http-body-util-0.1.2
  (crate-source "http-body-util" "0.1.2"
                "0kslwazg4400qnc2azkrgqqci0fppv12waicnsy5d8hncvbjjd3r"))

(define rust-httparse-1.8.0
  (crate-source "httparse" "1.8.0"
                "010rrfahm1jss3p022fqf3j3jmm72vhn4iqhykahb9ynpaag75yq"))

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

(define rust-inotify-0.11.0
  (crate-source "inotify" "0.11.0"
                "1wq8m657rl085cg59p38sc5y62xy9yhhpvxbkd7n1awi4zzwqzgk"))

(define rust-inotify-sys-0.1.5
  ;; TODO: Check bundled sources.
  (crate-source "inotify-sys" "0.1.5"
                "1syhjgvkram88my04kv03s0zwa66mdwa5v7ddja3pzwvx2sh4p70"))

(define rust-itoa-1.0.9
  (crate-source "itoa" "1.0.9"
                "0f6cpb4yqzhkrhhg6kqsw3wnmmhdnnffi6r2xzy248gzi2v0l5dg"))

(define rust-js-sys-0.3.72
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.72"
                "1a8r61hbgw5kmscgj3g5pzg2ywlnswvljy0l592v0xdxlayz323a"))

(define rust-lazy-static-1.4.0
  (crate-source "lazy_static" "1.4.0"
                "0in6ikhw8mgl33wjv6q6xfrb5b9jr16q8ygjy803fay4zcisvaz2"))

(define rust-lexopt-0.3.0
  (crate-source "lexopt" "0.3.0"
                "00dlvik2ygw8z101vf3bfndcvxhp92v25sbzz6bdiwvxgxhlpzxs"))

(define rust-libc-0.2.161
  (crate-source "libc" "0.2.161"
                "1lc5s3zd0491x9zxrv2kvclai1my1spz950pkkyry4vwh318k54f"))

(define rust-libdav-0.9.1
  (crate-source "libdav" "0.9.1"
                "1646mcnalav3jiprn3xyslyncmcvn34jzw5qn0h4k1x0bppczqhm"))

(define rust-linux-raw-sys-0.4.5
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.4.5"
                "00w52pb2cb4b2880ksyzagmzbyjdmp9ac0w3qfvjv3453fnzvg2p"))

(define rust-lock-api-0.4.10
  (crate-source "lock_api" "0.4.10"
                "05nd9nzxqidg24d1k8y5vlc8lz9gscpskrikycib46qbl8brgk61"))

(define rust-log-0.4.20
  (crate-source "log" "0.4.20"
                "13rf7wphnwd61vazpxr7fiycin6cb1g8fmvgqg18i464p0y1drmm"))

(define rust-memchr-2.6.2
  (crate-source "memchr" "2.6.2"
                "0vnqrsfm5260gcxyb83ipfd68d51l3azpm81i8dyc6320b8ax1jl"))

(define rust-mime-0.3.17
  (crate-source "mime" "0.3.17"
                "16hkibgvb9klh0w0jk5crr5xv90l3wlf77ggymzjmvl1818vnxv8"))

(define rust-miniz-oxide-0.7.1
  (crate-source "miniz_oxide" "0.7.1"
                "1ivl3rbbdm53bzscrd01g60l46lz5krl270487d8lhjvwl5hx0g7"))

(define rust-mio-1.0.2
  (crate-source "mio" "1.0.2"
                "1v1cnnn44awxbcfm4zlavwgkvbyg7gp5zzjm8mqf1apkrwflvq40"))

(define rust-moka-0.12.8
  (crate-source "moka" "0.12.8"
                "0vrbsd86bdnliwgnzwqw6gi3x7n4fl8gnck4wzfx4xfr9pmn5krj"))

(define rust-object-0.32.0
  (crate-source "object" "0.32.0"
                "1ghynapcbgzrmnbwmmxj129dbzvmh0hwx8bplmh8ra5f0yympb3p"))

(define rust-octseq-0.5.2
  (crate-source "octseq" "0.5.2"
                "04pycbrcxlmhxqmrs4jgd0kqjk9pwjil6zr4fp2wwi4wgjikqv0j"))

(define rust-once-cell-1.18.0
  (crate-source "once_cell" "1.18.0"
                "0vapcd5ambwck95wyz3ymlim35jirgnqn9a0qmi19msymv95v2yx"))

(define rust-openssl-probe-0.1.5
  (crate-source "openssl-probe" "0.1.5"
                "1kq18qm48rvkwgcggfkqq6pm948190czqc94d6bm2sir5hq1l0gz"))

(define rust-parking-2.2.1
  (crate-source "parking" "2.2.1"
                "1fnfgmzkfpjd69v4j9x737b1k8pnn054bvzcn5dm3pkgq595d3gk"))

(define rust-parking-lot-0.12.1
  (crate-source "parking_lot" "0.12.1"
                "13r2xk7mnxfc5g0g6dkdxqdqad99j7s7z8zhzz4npw5r0g0v4hip"))

(define rust-parking-lot-core-0.9.8
  (crate-source "parking_lot_core" "0.9.8"
                "0ixlak319bpzldq20yvyfqk0y1vi736zxbw101jvzjp7by30rw4k"))

(define rust-pin-project-1.1.5
  (crate-source "pin-project" "1.1.5"
                "1cxl146x0q7lawp0m1826wsgj8mmmfs6ja8q7m6f7ff5j6vl7gxn"))

(define rust-pin-project-internal-1.1.5
  (crate-source "pin-project-internal" "1.1.5"
                "0r9r4ivwiyqf45sv6b30l1dx282lxaax2f6gl84jwa3q590s8f1g"))

(define rust-pin-project-lite-0.2.13
  (crate-source "pin-project-lite" "0.2.13"
                "0n0bwr5qxlf0mhn2xkl36sy55118s9qmvx2yl5f3ixkb007lbywa"))

(define rust-pin-utils-0.1.0
  (crate-source "pin-utils" "0.1.0"
                "117ir7vslsl2z1a7qzhws4pd01cg2d3338c47swjyvqv2n60v1wb"))

(define rust-pkg-config-0.3.29
  (crate-source "pkg-config" "0.3.29"
                "1jy6158v1316khkpmq2sjj1vgbnbnw51wffx7p0k0l9h9vlys019"))

(define rust-ppv-lite86-0.2.17
  (crate-source "ppv-lite86" "0.2.17"
                "1pp6g52aw970adv3x2310n7glqnji96z0a9wiamzw89ibf0ayh2v"))

(define rust-proc-macro2-1.0.86
  (crate-source "proc-macro2" "1.0.86"
                "0xrv22p8lqlfdf1w0pj4si8n2ws4aw0kilmziwf0vpv5ys6rwway"))

(define rust-quanta-0.12.3
  (crate-source "quanta" "0.12.3"
                "19cds3yg3ri0wrypn7b3j2x8qf1w9rkw5yl4nah2i4k1fyj6flcf"))

(define rust-quote-1.0.36
  (crate-source "quote" "1.0.36"
                "19xcmh445bg6simirnnd4fvkmp6v2qiwxh5f6rw4a70h76pnm9qg"))

(define rust-rand-0.8.5
  (crate-source "rand" "0.8.5"
                "013l6931nn7gkc23jz5mm3qdhf93jjf0fg64nz2lp4i51qd8vbrl"))

(define rust-rand-chacha-0.3.1
  (crate-source "rand_chacha" "0.3.1"
                "123x2adin558xbhvqb8w4f6syjsdkmqff8cxwhmjacpsl1ihmhg6"))

(define rust-rand-core-0.6.4
  (crate-source "rand_core" "0.6.4"
                "0b4j2v4cb5krak1pv6kakv4sz6xcwbrmy2zckc32hsigbrwy82zc"))

(define rust-raw-cpuid-11.2.0
  (crate-source "raw-cpuid" "11.2.0"
                "1c77cmsn7rj6knwwrg2y9nl46wss5p9jq3wzxvr1a5k6bhql1chs"))

(define rust-redox-syscall-0.3.5
  (crate-source "redox_syscall" "0.3.5"
                "0acgiy2lc1m2vr8cr33l5s7k9wzby8dybyab1a9p753hcbr68xjn"))

(define rust-ring-0.17.14
  (crate-source "ring" "0.17.14"
                "1dw32gv19ccq4hsx3ribhpdzri1vnrlcfqb2vj41xn4l49n9ws54"))

(define rust-roxmltree-0.20.0
  (crate-source "roxmltree" "0.20.0"
                "15vw91ps91wkmmgy62khf9zb63bdinvm80957dascbsw7dwvc83c"))

(define rust-rustc-demangle-0.1.23
  (crate-source "rustc-demangle" "0.1.23"
                "0xnbk2bmyzshacjm2g1kd4zzv2y2az14bw3sjccq5qkpmsfvn9nn"))

(define rust-rustc-version-0.4.1
  (crate-source "rustc_version" "0.4.1"
                "14lvdsmr5si5qbqzrajgb6vfn69k0sfygrvfvr2mps26xwi3mjyg"))

(define rust-rustix-0.38.10
  (crate-source "rustix" "0.38.10"
                "0r096k86c9sbvirfnz5vy8k8qphkkwahcvi6irqfn9d6rbhlhqpd"))

(define rust-rustls-0.22.4
  (crate-source "rustls" "0.22.4"
                "0cl4q6w0x1cl5ldjsgbbiiqhkz6qg5vxl5dkn9wwsyxc44vzfkmz"))

(define rust-rustls-native-certs-0.7.0
  (crate-source "rustls-native-certs" "0.7.0"
                "14ip15dcr6fmjzi12lla9cpln7mmkdid4a7wsp344v4kz9gbh7wg"))

(define rust-rustls-pemfile-2.0.0
  (crate-source "rustls-pemfile" "2.0.0"
                "1x34xidvzn4br2vl8f8xwmhgbjv4lmlb0ggv5whlnk4yl87rir1m"))

(define rust-rustls-pki-types-1.7.0
  (crate-source "rustls-pki-types" "1.7.0"
                "0banlc9xzwqrx8n0h4bd0igmq3z5hc72rn941lf22cp3gkkraqlp"))

(define rust-rustls-webpki-0.102.4
  (crate-source "rustls-webpki" "0.102.4"
                "0gmk2abk7y2cdppqlaqmnhcv690p19af9n66sjvw84z9j9z8yi7z"))

(define rust-scfg-0.3.1
  (crate-source "scfg" "0.3.1"
                "1xfqn2yy75jg0jzwh9x4bxfi575csgrjjym32sf93hhg9nmknf59"))

(define rust-schannel-0.1.22
  (crate-source "schannel" "0.1.22"
                "126zy5jb95fc5hvzyjwiq6lc81r08rdcn6affn00ispp9jzk6dqc"))

(define rust-scopeguard-1.2.0
  (crate-source "scopeguard" "1.2.0"
                "0jcz9sd47zlsgcnm1hdw0664krxwb5gczlif4qngj2aif8vky54l"))

(define rust-security-framework-2.9.2
  (crate-source "security-framework" "2.9.2"
                "1pplxk15s5yxvi2m1sz5xfmjibp96cscdcl432w9jzbk0frlzdh5"))

(define rust-security-framework-sys-2.9.1
  ;; TODO: Check bundled sources.
  (crate-source "security-framework-sys" "2.9.1"
                "0yhciwlsy9dh0ps1gw3197kvyqx1bvc4knrhiznhid6kax196cp9"))

(define rust-semver-1.0.23
  (crate-source "semver" "1.0.23"
                "12wqpxfflclbq4dv8sa6gchdh92ahhwn4ci1ls22wlby3h57wsb1"))

(define rust-serde-1.0.188
  (crate-source "serde" "1.0.188"
                "17jlqzfhimsk8w37ifjwnm86nwjzawlbgwmwc7nhwdwslv5hz7ng"))

(define rust-serde-derive-1.0.188
  (crate-source "serde_derive" "1.0.188"
                "1wjaclvsfxgqnnnykllvb5gffsxynk66x6h4c1ds6anq8b37mjjf"))

(define rust-sha2-0.10.7
  (crate-source "sha2" "0.10.7"
                "1n3flx8bjyblmb2n860g8402z7q10caajp2n403n37i3cbcbk7s7"))

(define rust-shell-words-1.1.0
  (crate-source "shell-words" "1.1.0"
                "1plgwx8r0h5ismbbp6cp03740wmzgzhip85k5hxqrrkaddkql614"))

(define rust-shlex-1.3.0
  (crate-source "shlex" "1.3.0"
                "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg"))

(define rust-signal-hook-registry-1.4.1
  (crate-source "signal-hook-registry" "1.4.1"
                "18crkkw5k82bvcx088xlf5g4n3772m24qhzgfan80nda7d3rn8nq"))

(define rust-simple-logger-4.2.0
  (crate-source "simple_logger" "4.2.0"
                "0cwkcbd8ba73ic8g1n896n7mhrgdlm4hnqgvk6vcj5dq55fcsc12"))

(define rust-slab-0.4.9
  (crate-source "slab" "0.4.9"
                "0rxvsgir0qw5lkycrqgb1cxsvxzjv9bmx73bk5y42svnzfba94lg"))

(define rust-smallvec-1.13.2
  (crate-source "smallvec" "1.13.2"
                "0rsw5samawl3wsw6glrsb127rx6sh89a8wyikicw6dkdcjd1lpiw"))

(define rust-socket2-0.5.7
  (crate-source "socket2" "0.5.7"
                "070r941wbq76xpy039an4pyiy3rfj7mp7pvibf1rcri9njq5wc6f"))

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

(define rust-subtle-2.5.0
  (crate-source "subtle" "2.5.0"
                "1g2yjs7gffgmdvkkq0wrrh0pxds3q0dv6dhkw9cdpbib656xdkc1"))

(define rust-syn-2.0.67
  (crate-source "syn" "2.0.67"
                "144c0apb6zqqlxdyiiy42ynlahn1ddw66cpxwd7azww63pnmb1pz"))

(define rust-tagptr-0.2.0
  (crate-source "tagptr" "0.2.0"
                "05r4mwvlsclx1ayj65hpzjv3dn4wpi8j4xm695vydccf9k7r683v"))

(define rust-tempfile-3.8.0
  (crate-source "tempfile" "3.8.0"
                "1vsl2193w3gpx3mwj36fwx3v6q2qyvmzrdn6m8fgfsjkrkrx556b"))

(define rust-thiserror-1.0.47
  (crate-source "thiserror" "1.0.47"
                "13wdsrdyrq6x3rcydvxlx4mxck0c5v3mz1dj8zp7xhdg63n05a4p"))

(define rust-thiserror-impl-1.0.47
  (crate-source "thiserror-impl" "1.0.47"
                "16z1irxb45l011af53diap97x44dixnbp60v9g6pvarrdssj7dkb"))

(define rust-time-0.3.28
  (crate-source "time" "0.3.28"
                "0j3yl5q4w9vcw55hxxb1a3crls1w82v5dahicj7c4ifjgxavpxhp"))

(define rust-time-core-0.1.1
  (crate-source "time-core" "0.1.1"
                "1yz6d246zbmx9v6wpfg1jyfjlsgagirz7km96pr1mp6snkpzn03k"))

(define rust-tokio-1.41.0
  (crate-source "tokio" "1.41.0"
                "1fwb4nm630hmy9cyl2ar6wxqckgvsakwhg1rhjza4is3a09k8pql"))

(define rust-tokio-macros-2.4.0
  (crate-source "tokio-macros" "2.4.0"
                "0lnpg14h1v3fh2jvnc8cz7cjf0m7z1xgkwfpcyy632g829imjgb9"))

(define rust-tokio-rustls-0.25.0
  (crate-source "tokio-rustls" "0.25.0"
                "03w6d5aqqf084rmcmrsyq5grhydl53blaiqcl0i2yfnv187hqpkp"))

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

(define rust-tracing-attributes-0.1.27
  (crate-source "tracing-attributes" "0.1.27"
                "1rvb5dn9z6d0xdj14r403z0af0bbaqhg02hq4jc97g5wds6lqw1l"))

(define rust-tracing-core-0.1.32
  (crate-source "tracing-core" "0.1.32"
                "0m5aglin3cdwxpvbg6kz0r9r0k31j48n0kcfwsp6l49z26k3svf0"))

(define rust-triomphe-0.1.11
  (crate-source "triomphe" "0.1.11"
                "1crf71hndy3fc68x8v4aikkdjynp4n5sdhq28sck8x7frx8bd7l5"))

(define rust-try-lock-0.2.4
  (crate-source "try-lock" "0.2.4"
                "1vc15paa4zi06ixsxihwbvfn24d708nsyg1ncgqwcrn42byyqa1m"))

(define rust-typenum-1.16.0
  (crate-source "typenum" "1.16.0"
                "1fhb9iaqyjn4dzn2vl86kxjhp4xpw5gynczlnqzf4x6rjgpn2ya9"))

(define rust-unicode-ident-1.0.11
  (crate-source "unicode-ident" "1.0.11"
                "0g7wmn39nl9yzhjwn9ihacd22ymli8r4nlc2xf3idaas8ypbl6ih"))

(define rust-untrusted-0.9.0
  (crate-source "untrusted" "0.9.0"
                "1ha7ib98vkc538x0z60gfn0fc5whqdd85mb87dvisdcaifi6vjwf"))

(define rust-uuid-1.11.0
  (crate-source "uuid" "1.11.0"
                "0sj4l28lif2wm4xrafdfgqjywjzv43wzp8nii9a4i539myhg1igq"))

(define rust-version-check-0.9.4
  (crate-source "version_check" "0.9.4"
                "0gs8grwdlgh0xq660d7wr80x14vxbizmd8dbp29p2pdncx8lp1s9"))

(define rust-vparser-1.0.1
  (crate-source "vparser" "1.0.1"
                "0yjszxiqz9bwxd5qx4w8k1gcbgf1mi9wrk75d89443najyl3klzr"))

(define rust-want-0.3.1
  (crate-source "want" "0.3.1"
                "03hbfrnvqqdchb5kgxyavb9jabwza0dmh2vw5kg0dq8rxl57d9xz"))

(define rust-wasi-0.11.0+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.0+wasi-snapshot-preview1"
                "08z4hxwkpdpalxjps1ai9y7ihin26y9f476i53dv98v45gkqg3cw"))

(define rust-wasm-bindgen-0.2.95
  (crate-source "wasm-bindgen" "0.2.95"
                "0bpbvmxhil380gpv53smaypl8wc7sy7rq8apxfw349pn78v1x38j"))

(define rust-wasm-bindgen-backend-0.2.95
  (crate-source "wasm-bindgen-backend" "0.2.95"
                "0n53wgy78bgzgjwk0z69zbspzhv8p2a4zh69s4fzvpqdrb9x8vfb"))

(define rust-wasm-bindgen-macro-0.2.95
  (crate-source "wasm-bindgen-macro" "0.2.95"
                "0mic8b2vab1a91m6x3hjxkwz23094bq1cwhnszarsnlggyz894z7"))

(define rust-wasm-bindgen-macro-support-0.2.95
  (crate-source "wasm-bindgen-macro-support" "0.2.95"
                "0s7g6glb85lyx2pj83shbmg4d50mvqhb2c2qk2j28yigaxbspii6"))

(define rust-wasm-bindgen-shared-0.2.95
  (crate-source "wasm-bindgen-shared" "0.2.95"
                "1386q7mvv5ky003hcc6yyxpid3y1m7fy0l920i3z3ab60vqhkz35"))

(define rust-web-sys-0.3.72
  ;; TODO: Check bundled sources.
  (crate-source "web-sys" "0.3.72"
                "04k19hilj9r8sx6q20fz853149gfpmf83yk2zvq0s14c2288nj7n"))

(define rust-winapi-0.3.9
  (crate-source "winapi" "0.3.9"
                "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))

(define rust-winapi-i686-pc-windows-gnu-0.4.0
  (crate-source "winapi-i686-pc-windows-gnu" "0.4.0"
                "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))

(define rust-winapi-x86-64-pc-windows-gnu-0.4.0
  (crate-source "winapi-x86_64-pc-windows-gnu" "0.4.0"
                "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))

(define rust-windows-aarch64-gnullvm-0.42.2
  (crate-source "windows_aarch64_gnullvm" "0.42.2"
                "1y4q0qmvl0lvp7syxvfykafvmwal5hrjb4fmv04bqs0bawc52yjr"))

(define rust-windows-aarch64-gnullvm-0.48.5
  (crate-source "windows_aarch64_gnullvm" "0.48.5"
                "1n05v7qblg1ci3i567inc7xrkmywczxrs1z3lj3rkkxw18py6f1b"))

(define rust-windows-aarch64-gnullvm-0.52.6
  (crate-source "windows_aarch64_gnullvm" "0.52.6"
                "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"))

(define rust-windows-aarch64-msvc-0.42.2
  (crate-source "windows_aarch64_msvc" "0.42.2"
                "0hsdikjl5sa1fva5qskpwlxzpc5q9l909fpl1w6yy1hglrj8i3p0"))

(define rust-windows-aarch64-msvc-0.48.5
  (crate-source "windows_aarch64_msvc" "0.48.5"
                "1g5l4ry968p73g6bg6jgyvy9lb8fyhcs54067yzxpcpkf44k2dfw"))

(define rust-windows-aarch64-msvc-0.52.6
  (crate-source "windows_aarch64_msvc" "0.52.6"
                "0sfl0nysnz32yyfh773hpi49b1q700ah6y7sacmjbqjjn5xjmv09"))

(define rust-windows-i686-gnu-0.42.2
  (crate-source "windows_i686_gnu" "0.42.2"
                "0kx866dfrby88lqs9v1vgmrkk1z6af9lhaghh5maj7d4imyr47f6"))

(define rust-windows-i686-gnu-0.48.5
  (crate-source "windows_i686_gnu" "0.48.5"
                "0gklnglwd9ilqx7ac3cn8hbhkraqisd0n83jxzf9837nvvkiand7"))

(define rust-windows-i686-gnu-0.52.6
  (crate-source "windows_i686_gnu" "0.52.6"
                "02zspglbykh1jh9pi7gn8g1f97jh1rrccni9ivmrfbl0mgamm6wf"))

(define rust-windows-i686-gnullvm-0.52.6
  (crate-source "windows_i686_gnullvm" "0.52.6"
                "0rpdx1537mw6slcpqa0rm3qixmsb79nbhqy5fsm3q2q9ik9m5vhf"))

(define rust-windows-i686-msvc-0.42.2
  (crate-source "windows_i686_msvc" "0.42.2"
                "0q0h9m2aq1pygc199pa5jgc952qhcnf0zn688454i7v4xjv41n24"))

(define rust-windows-i686-msvc-0.48.5
  (crate-source "windows_i686_msvc" "0.48.5"
                "01m4rik437dl9rdf0ndnm2syh10hizvq0dajdkv2fjqcywrw4mcg"))

(define rust-windows-i686-msvc-0.52.6
  (crate-source "windows_i686_msvc" "0.52.6"
                "0rkcqmp4zzmfvrrrx01260q3xkpzi6fzi2x2pgdcdry50ny4h294"))

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

(define rust-windows-targets-0.48.5
  (crate-source "windows-targets" "0.48.5"
                "034ljxqshifs1lan89xwpcy1hp0lhdh4b5n0d2z4fwjx2piacbws"))

(define rust-windows-targets-0.52.6
  (crate-source "windows-targets" "0.52.6"
                "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))

(define rust-windows-x86-64-gnu-0.42.2
  (crate-source "windows_x86_64_gnu" "0.42.2"
                "0dnbf2xnp3xrvy8v9mgs3var4zq9v9yh9kv79035rdgyp2w15scd"))

(define rust-windows-x86-64-gnu-0.48.5
  (crate-source "windows_x86_64_gnu" "0.48.5"
                "13kiqqcvz2vnyxzydjh73hwgigsdr2z1xpzx313kxll34nyhmm2k"))

(define rust-windows-x86-64-gnu-0.52.6
  (crate-source "windows_x86_64_gnu" "0.52.6"
                "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"))

(define rust-windows-x86-64-gnullvm-0.42.2
  (crate-source "windows_x86_64_gnullvm" "0.42.2"
                "18wl9r8qbsl475j39zvawlidp1bsbinliwfymr43fibdld31pm16"))

(define rust-windows-x86-64-gnullvm-0.48.5
  (crate-source "windows_x86_64_gnullvm" "0.48.5"
                "1k24810wfbgz8k48c2yknqjmiigmql6kk3knmddkv8k8g1v54yqb"))

(define rust-windows-x86-64-gnullvm-0.52.6
  (crate-source "windows_x86_64_gnullvm" "0.52.6"
                "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"))

(define rust-windows-x86-64-msvc-0.42.2
  (crate-source "windows_x86_64_msvc" "0.42.2"
                "1w5r0q0yzx827d10dpjza2ww0j8iajqhmb54s735hhaj66imvv4s"))

(define rust-windows-x86-64-msvc-0.48.5
  (crate-source "windows_x86_64_msvc" "0.48.5"
                "0f4mdp895kkjh9zv8dxvn4pc10xr7839lf5pa9l0193i2pkgr57d"))

(define rust-windows-x86-64-msvc-0.52.6
  (crate-source "windows_x86_64_msvc" "0.52.6"
                "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq"))

(define rust-zeroize-1.7.0
  (crate-source "zeroize" "1.7.0"
                "0bfvby7k9pdp6623p98yz2irqnamcyzpn7zh20nqmdn68b0lwnsj"))

(define-cargo-inputs lookup-cargo-inputs
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
