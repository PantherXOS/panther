;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>
;;;
;;; guix import -i px/packages/rust-crates.scm crate -f /path/to/Cargo.lock PACKAGE

(define-module (px packages rust-crates)
  #:use-module (guix build-system cargo)
  #:export (lookup-cargo-inputs))

(define rust-addr2line-0.21.0
  (crate-source "addr2line" "0.21.0"
                "1jx0k3iwyqr8klqbzk6kjvr496yd94aspis10vwsj5wy7gib4c4a"))

(define rust-adler-1.0.2
  (crate-source "adler" "1.0.2"
                "1zim79cvzd5yrkzl3nyfx0avijwgk9fqv3yrscdy1cc79ih02qpj"))

(define rust-adler2-2.0.1
  (crate-source "adler2" "2.0.1"
                "1ymy18s9hs7ya1pjc9864l30wk8p2qfqdi7mhhcc5nfakxbij09j"))

(define rust-ahash-0.8.12
  (crate-source "ahash" "0.8.12"
                "0xbsp9rlm5ki017c0w6ay8kjwinwm8knjncci95mii30rmwz25as"))

(define rust-aho-corasick-1.1.3
  (crate-source "aho-corasick" "1.1.3"
                "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f"))

(define rust-aligned-vec-0.6.4
  (crate-source "aligned-vec" "0.6.4"
                "16vnf78hvfix5cwzd5xs5a2g6afmgb4h7n6yfsc36bv0r22072fw"))

(define rust-allocator-api2-0.2.21
  (crate-source "allocator-api2" "0.2.21"
                "08zrzs022xwndihvzdn78yqarv2b9696y67i6h78nla3ww87jgb8"))

(define rust-android-system-properties-0.1.5
  (crate-source "android_system_properties" "0.1.5"
                "04b3wrz12837j7mdczqd95b732gw5q7q66cv4yn4646lvccp57l1"))

(define rust-ansi-colours-1.2.3
  (crate-source "ansi_colours" "1.2.3"
                "1zimwh84gs1r0g0chy6x5lm9v0ksxxlzwy8nyj80f6cq08zc9vhl"))

(define rust-anstream-0.6.20
  (crate-source "anstream" "0.6.20"
                "14k1iqdf3dx7hdjllmql0j9sjxkwr1lfdddi3adzff0r7mjn7r9s"))

(define rust-anstyle-1.0.13
  (crate-source "anstyle" "1.0.13"
                "0y2ynjqajpny6q0amvfzzgw0gfw3l47z85km4gvx87vg02lcr4ji"))

(define rust-anstyle-parse-0.2.7
  (crate-source "anstyle-parse" "0.2.7"
                "1hhmkkfr95d462b3zf6yl2vfzdqfy5726ya572wwg8ha9y148xjf"))

(define rust-anstyle-query-1.1.4
  (crate-source "anstyle-query" "1.1.4"
                "1qir6d6fl5a4y2gmmw9a5w93ckwx6xn51aryd83p26zn6ihiy8wy"))

(define rust-anstyle-wincon-3.0.10
  (crate-source "anstyle-wincon" "3.0.10"
                "0ajz9wsf46a2l3pds7v62xbhq2cffj7wrilamkx2z8r28m0k61iy"))

(define rust-anyhow-1.0.100
  (crate-source "anyhow" "1.0.100"
                "0qbfmw4hhv2ampza1csyvf1jqjs2dgrj29cv3h3sh623c6qvcgm2"))

(define rust-anyhow-1.0.75
  (crate-source "anyhow" "1.0.75"
                "1rmcjkim91c5mw7h9wn8nv0k6x118yz0xg0z1q18svgn42mqqrm4"))

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

(define rust-arrayref-0.3.9
  (crate-source "arrayref" "0.3.9"
                "1jzyp0nvp10dmahaq9a2rnxqdd5wxgbvp8xaibps3zai8c9fi8kn"))

(define rust-arrayvec-0.7.6
  (crate-source "arrayvec" "0.7.6"
                "0l1fz4ccgv6pm609rif37sl5nv5k6lbzi7kkppgzqzh1vwix20kw"))

(define rust-async-lock-3.4.0
  (crate-source "async-lock" "3.4.0"
                "060vh45i809wcqyxzs5g69nqiqah7ydz0hpkcjys9258vqn4fvpz"))

(define rust-async-trait-0.1.73
  (crate-source "async-trait" "0.1.73"
                "1w60x18qm18drm8pdl0ix4jai83nh8hlwfjswca3dh4096rww05w"))

(define rust-autocfg-1.1.0
  (crate-source "autocfg" "1.1.0"
                "1ylp3cb47ylzabimazvbz9ms6ap784zhb6syaz6c1jqpmcmq0s6l"))

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

(define rust-base64-0.13.1
  (crate-source "base64" "0.13.1"
                "1s494mqmzjb766fy1kqlccgfg2sdcjb6hzbvzqv2jw65fdi5h6wy"))

(define rust-base64-0.21.3
  (crate-source "base64" "0.21.3"
                "0lvf1ishhckkjwiamhqr3iwy5ddrzgvgqfkblwkcaxrxqvxwwka1"))

(define rust-base64-0.22.1
  (crate-source "base64" "0.22.1"
                "1imqzgh7bxcikp5vx3shqvw9j09g9ly0xr0jma0q66i52r7jbcvj"))

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

(define rust-bitflags-1.3.2
  (crate-source "bitflags" "1.3.2"
                "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))

(define rust-bitflags-2.4.0
  (crate-source "bitflags" "2.4.0"
                "0dc6xa7flfl59makmhixjcrslwlvdxxwrgxbr8p7bkvz53k2ls5l"))

(define rust-bitflags-2.9.4
  (crate-source "bitflags" "2.9.4"
                "157kkcv8s7vk6d17dar1pa5cqcz4c8pdrn16wm1ld7jnr86d2q92"))

(define rust-bitstream-io-2.6.0
  (crate-source "bitstream-io" "2.6.0"
                "1cli390l1dhp9skygyjjnqvczp36b7f31mkx9ry3dg26330cv6b0"))

(define rust-block-buffer-0.10.4
  (crate-source "block-buffer" "0.10.4"
                "0w9sa2ypmrsqqvc20nhwr75wbb5cjr4kkyhpjm1z1lv2kdicfy1h"))

(define rust-block2-0.5.1
  (crate-source "block2" "0.5.1"
                "0pyiha5his2grzqr3mynmq244laql2j20992i59asp0gy7mjw4rc"))

(define rust-bstr-1.12.0
  (crate-source "bstr" "1.12.0"
                "195i0gd7r7jg7a8spkmw08492n7rmiabcvz880xn2z8dkp8i6h93"))

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

(define rust-byteorder-lite-0.1.0
  (crate-source "byteorder-lite" "0.1.0"
                "15alafmz4b9az56z6x7glcbcb6a8bfgyd109qc3bvx07zx4fj7wg"))

(define rust-bytes-1.4.0
  (crate-source "bytes" "1.4.0"
                "1gkh3fk4fm9xv5znlib723h5md5sxsvbd5113sbxff6g1lmgvcl9"))

(define rust-camino-1.1.6
  (crate-source "camino" "1.1.6"
                "171vzfyrm2jmajd70q1m774297y028kadgm7cfw4kxc8lfsr57n5"))

(define rust-cc-1.2.16
  (crate-source "cc" "1.2.16"
                "131bhgafc1i86vvjipkj0kwzz0hlpwrkl8mdbmzyq2g69calqwdy"))

(define rust-cc-1.2.39
  (crate-source "cc" "1.2.39"
                "0py3546wz3k5qi6pbfz80jvg0g3qgzr21c7a1p5wjvscjm4l6dg1"))

(define rust-cfg-aliases-0.2.1
  (crate-source "cfg_aliases" "0.2.1"
                "092pxdc1dbgjb6qvh83gk56rkic2n2ybm4yvy76cgynmzi3zwfk1"))

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

(define rust-chrono-0.4.42
  (crate-source "chrono" "0.4.42"
                "1lp8iz9js9jwxw0sj8yi59v54lgvwdvm49b9wch77f25sfym4l0l"))

(define rust-clap-4.5.48
  (crate-source "clap" "4.5.48"
                "1bjz3d7bavy13ph2a6rm3c9y02ak70b195xakii7h6q2xarln4z2"))

(define rust-clap-builder-4.5.48
  (crate-source "clap_builder" "4.5.48"
                "1jaxnr7ik25r4yxgz657vm8kz62f64qmwxhplmzxz9n0lfpn9fn2"))

(define rust-clap-complete-4.5.58
  (crate-source "clap_complete" "4.5.58"
                "0jmg0idg96cvx51l35ypia1np3q7sfj5wqxvi7kjs59fmlr0pgvm"))

(define rust-clap-derive-4.5.47
  (crate-source "clap_derive" "4.5.47"
                "174z9g13s85la2nmi8gv8ssjwz77im3rqg5isiinw6hg1fp7xzdv"))

(define rust-clap-help-1.5.0
  (crate-source "clap-help" "1.5.0"
                "0nwqnry884f6vky61y8vjkynyya4yggnz7gh7p9rq86s7bknxn59"))

(define rust-clap-lex-0.7.5
  (crate-source "clap_lex" "0.7.5"
                "0xb6pjza43irrl99axbhs12pxq4sr8x7xd36p703j57f5i3n2kxr"))

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

(define rust-color-quant-1.1.0
  (crate-source "color_quant" "1.1.0"
                "12q1n427h2bbmmm1mnglr57jaz2dj9apk0plcxw7nwqiai7qjyrx"))

(define rust-colorchoice-1.0.4
  (crate-source "colorchoice" "1.0.4"
                "0x8ymkz1xr77rcj1cfanhf416pc4v681gmkc9dzb3jqja7f62nxh"))

(define rust-colored-2.1.0
  (crate-source "colored" "2.1.0"
                "1f4h9p64snrnz4x432iza15p4diqjcgpmpvhi956d6r1rq61bwnb"))

(define rust-concurrent-queue-2.5.0
  (crate-source "concurrent-queue" "2.5.0"
                "0wrr3mzq2ijdkxwndhf79k952cp4zkz35ray8hvsxl96xrx1k82c"))

(define rust-convert-case-0.7.1
  (crate-source "convert_case" "0.7.1"
                "1rzih8qbd3xh87wp76nkjvnrimn7vlzcwl2n88898ml59j6jnh5v"))

(define rust-coolor-1.1.0
  (crate-source "coolor" "1.1.0"
                "1wr7q2c8l1cmigw3h7yfdpwcz5g5xbwkirsvbjhdchxgwkyjl34q"))

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

(define rust-cpufeatures-0.2.9
  (crate-source "cpufeatures" "0.2.9"
                "1wg1vmsx3gd30xkc7h7r6nfx7njx063hqjimgyrb0qj17bzpcyx1"))

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

(define rust-crypto-common-0.1.6
  (crate-source "crypto-common" "0.1.6"
                "1cvby95a6xg7kxdz5ln3rl9xh66nz66w46mm3g56ri1z5x815yqv"))

(define rust-csv-1.3.1
  (crate-source "csv" "1.3.1"
                "1bzxgbbhy27flcyafxbj7f1hbn7b8wac04ijfgj34ry9m61lip5c"))

(define rust-csv-core-0.1.12
  (crate-source "csv-core" "0.1.12"
                "0gfrjjlfagarhyclxrqv6b14iaxgvgc8kmwwdvw08racvaqg60kx"))

(define rust-csv2svg-0.2.3
  (crate-source "csv2svg" "0.2.3"
                "0wf1ybnnrp85gnygbgi5s5inzm9yyzrc284nivxwhw4mrzgadhhw"))

(define rust-custom-error-1.9.2
  (crate-source "custom_error" "1.9.2"
                "19mwa90z1hgwn3mqj07b4cy6j4yc8c59k2n99mdvm9kz37fm32jg"))

(define rust-data-url-0.3.2
  (crate-source "data-url" "0.3.2"
                "0xl30jidc8s3kh2z3nvnn1nyzhbq5b2wpiqwzj9gjdrndk50n7my"))

(define rust-deranged-0.3.8
  (crate-source "deranged" "0.3.8"
                "0ikrhil2621rz9haakphdzrx035qwr175f639p8qyrazjj56wsgj"))

(define rust-deranged-0.5.4
  (crate-source "deranged" "0.5.4"
                "0wch36gpg2crz2f72p7c0i5l4bzxjkwxw96sdj57c1cadzw566d4"))

(define rust-derive-more-2.0.1
  (crate-source "derive_more" "2.0.1"
                "0y3n97cc7rsvgnj211p92y1ppzh6jzvq5kvk6340ghkhfp7l4ch9"))

(define rust-derive-more-impl-2.0.1
  (crate-source "derive_more-impl" "2.0.1"
                "1wqxcb7d5lzvpplz9szp4rwy1r23f5wmixz0zd2vcjscqknji9mx"))

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

(define rust-dirs-sys-0.3.7
  ;; TODO: Check bundled sources.
  (crate-source "dirs-sys" "0.3.7"
                "19md1cnkazham8a6kh22v12d8hh3raqahfk6yb043vrjr68is78v"))

(define rust-dirs-sys-0.4.1
  ;; TODO: Check bundled sources.
  (crate-source "dirs-sys" "0.4.1"
                "071jy0pvaad9lsa6mzawxrh7cmr7hsmsdxwzm7jzldfkrfjha3sj"))

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

(define rust-either-1.15.0
  (crate-source "either" "1.15.0"
                "069p1fknsmzn9llaizh77kip0pqmcwpdsykv2x30xpjyija5gis8"))

(define rust-equator-0.4.2
  (crate-source "equator" "0.4.2"
                "1z760z5r0haxjyakbqxvswrz9mq7c29arrivgq8y1zldhc9v44a7"))

(define rust-equator-macro-0.4.2
  (crate-source "equator-macro" "0.4.2"
                "1cqzx3cqn9rxln3a607xr54wippzff56zs5chqdf3z2bnks3rwj4"))

(define rust-equivalent-1.0.2
  (crate-source "equivalent" "1.0.2"
                "03swzqznragy8n0x31lqc78g2af054jwivp7lkrbrc0khz74lyl7"))

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

(define rust-event-listener-strategy-0.5.2
  (crate-source "event-listener-strategy" "0.5.2"
                "18f5ri227khkayhv3ndv7yl4rnasgwksl2jhwgafcxzr7324s88g"))

(define rust-exr-1.73.0
  (crate-source "exr" "1.73.0"
                "1q47yq78q9k210r6jy1wwrilxwwxqavik9l3l426rd17k7srfcgq"))

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

(define rust-file-size-1.0.3
  (crate-source "file-size" "1.0.3"
                "1cyj7067fs7ml8pjrwzjy3qrns3yxaxakf0na1v5fffk0l0z2i4m"))

(define rust-find-msvc-tools-0.1.2
  (crate-source "find-msvc-tools" "0.1.2"
                "0nbrhvk4m04hviiwbqp2jwcv9j2k70x0q2kcvfk51iygvaqp7v8w"))

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

(define rust-form-urlencoded-1.2.2
  (crate-source "form_urlencoded" "1.2.2"
                "1kqzb2qn608rxl3dws04zahcklpplkd5r1vpabwga5l50d2v4k6b"))

(define rust-fsevent-sys-4.1.0
  ;; TODO: Check bundled sources.
  (crate-source "fsevent-sys" "4.1.0"
                "1liz67v8b0gcs8r31vxkvm2jzgl9p14i78yfqx81c8sdv817mvkn"))

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

(define rust-gethostname-0.3.0
  (crate-source "gethostname" "0.3.0"
                "0xy1wbx5k2bzi4cbaqj9wqgqsbn4f8pm6nsm1d86mibk66xd8rdv"))

(define rust-getrandom-0.2.10
  (crate-source "getrandom" "0.2.10"
                "09zlimhhskzf7cmgcszix05wyz2i6fcpvh711cv1klsxl6r3chdy"))

(define rust-getrandom-0.2.16
  (crate-source "getrandom" "0.2.16"
                "14l5aaia20cc6cc08xdlhrzmfcylmrnprwnna20lqf746pqzjprk"))

(define rust-getrandom-0.3.3
  (crate-source "getrandom" "0.3.3"
                "1x6jl875zp6b2b6qp9ghc84b0l76bvng2lvm8zfcmwjl7rb5w516"))

(define rust-gif-0.13.3
  (crate-source "gif" "0.13.3"
                "06z6gll24q7psbz9fb86jbcbmgwnxkym8jsp0fbq5qikbqilgq2a"))

(define rust-gimli-0.28.0
  (crate-source "gimli" "0.28.0"
                "1h7hcl3chfvd2gfrrxjymnwj7anqxjslvz20kcargkvsya2dgf3g"))

(define rust-git2-0.20.2
  (crate-source "git2" "0.20.2"
                "0451zzmvblvlrj6y6pgdsxrqh42hi789n3k9lp0hslmi6fhhgsrd"))

(define rust-glassbench-0.4.4
  (crate-source "glassbench" "0.4.4"
                "0459fsmfhdvg2z1085a6gnydl1y8s3xanrr9l3vqqd0s51q7hqsa"))

(define rust-glob-0.3.3
  (crate-source "glob" "0.3.3"
                "106jpd3syfzjfj2k70mwm0v436qbx96wig98m4q8x071yrq35hhc"))

(define rust-half-2.6.0
  (crate-source "half" "2.6.0"
                "1j83v0xaqvrw50ppn0g33zig0zsbdi7xiqbzgn7sd5al57nrd4a5"))

(define rust-hashbrown-0.14.5
  (crate-source "hashbrown" "0.14.5"
                "1wa1vy1xs3mp11bn3z9dv0jricgr6a2j0zkf1g19yz3vw4il89z5"))

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

(define rust-hermit-abi-0.3.9
  (crate-source "hermit-abi" "0.3.9"
                "092hxjbjnq5fmz66grd9plxd0sh6ssg5fhgwwwqbrzgzkjwdycfj"))

(define rust-hex-0.4.3
  (crate-source "hex" "0.4.3"
                "0w1a4davm1lgzpamwnba907aysmlrnygbqmfis2mqjx5m552a93z"))

(define rust-home-0.5.11
  (crate-source "home" "0.5.11"
                "1kxb4k87a9sayr8jipr7nq9wpgmjk4hk4047hmf9kc24692k75aq"))

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

(define rust-iana-time-zone-0.1.64
  (crate-source "iana-time-zone" "0.1.64"
                "1yz980fmhaq9bdkasz35z63az37ci6kzzfhya83kgdqba61pzr9k"))

(define rust-iana-time-zone-haiku-0.1.2
  (crate-source "iana-time-zone-haiku" "0.1.2"
                "17r6jmj31chn7xs9698r122mapq85mfnv98bb4pg6spm0si2f67k"))

(define rust-icu-collections-2.0.0
  (crate-source "icu_collections" "2.0.0"
                "0izfgypv1hsxlz1h8fc2aak641iyvkak16aaz5b4aqg3s3sp4010"))

(define rust-icu-locale-core-2.0.0
  (crate-source "icu_locale_core" "2.0.0"
                "02phv7vwhyx6vmaqgwkh2p4kc2kciykv2px6g4h8glxfrh02gphc"))

(define rust-icu-normalizer-2.0.0
  (crate-source "icu_normalizer" "2.0.0"
                "0ybrnfnxx4sf09gsrxri8p48qifn54il6n3dq2xxgx4dw7l80s23"))

(define rust-icu-normalizer-data-2.0.0
  (crate-source "icu_normalizer_data" "2.0.0"
                "1lvjpzxndyhhjyzd1f6vi961gvzhj244nribfpdqxjdgjdl0s880"))

(define rust-icu-properties-2.0.1
  (crate-source "icu_properties" "2.0.1"
                "0az349pjg8f18lrjbdmxcpg676a7iz2ibc09d2wfz57b3sf62v01"))

(define rust-icu-properties-data-2.0.1
  (crate-source "icu_properties_data" "2.0.1"
                "0cnn3fkq6k88w7p86w7hsd1254s4sl783rpz4p6hlccq74a5k119"))

(define rust-icu-provider-2.0.0
  (crate-source "icu_provider" "2.0.0"
                "1bz5v02gxv1i06yhdhs2kbwxkw3ny9r2vvj9j288fhazgfi0vj03"))

(define rust-id-arena-2.2.1
  (crate-source "id-arena" "2.2.1"
                "01ch8jhpgnih8sawqs44fqsqpc7bzwgy0xpi6j0f4j0i5mkvr8i5"))

(define rust-idna-1.1.0
  (crate-source "idna" "1.1.0"
                "1pp4n7hppm480zcx411dsv9wfibai00wbpgnjj4qj0xa7kr7a21v"))

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

(define rust-imgref-1.12.0
  (crate-source "imgref" "1.12.0"
                "1j3iwdal9mdkmyrsms3lz4n1bxxxjxss2jvbmh662fns63fcxig7"))

(define rust-include-dir-0.7.4
  (crate-source "include_dir" "0.7.4"
                "1pfh3g45z88kwq93skng0n6g3r7zkhq9ldqs9y8rvr7i11s12gcj"))

(define rust-include-dir-macros-0.7.4
  (crate-source "include_dir_macros" "0.7.4"
                "0x8smnf6knd86g69p19z5lpfsaqp8w0nx14kdpkz1m8bxnkqbavw"))

(define rust-indexmap-2.11.4
  (crate-source "indexmap" "2.11.4"
                "1rc8bgcjzfcskz1zipjjm7s3m1jskzhnhr9jxmsafhdk1xv863sb"))

(define rust-inotify-0.11.0
  (crate-source "inotify" "0.11.0"
                "1wq8m657rl085cg59p38sc5y62xy9yhhpvxbkd7n1awi4zzwqzgk"))

(define rust-inotify-sys-0.1.5
  ;; TODO: Check bundled sources.
  (crate-source "inotify-sys" "0.1.5"
                "1syhjgvkram88my04kv03s0zwa66mdwa5v7ddja3pzwvx2sh4p70"))

(define rust-interpolate-name-0.2.4
  (crate-source "interpolate_name" "0.2.4"
                "0q7s5mrfkx4p56dl8q9zq71y1ysdj4shh6f28qf9gly35l21jj63"))

(define rust-io-kit-sys-0.4.1
  ;; TODO: Check bundled sources.
  (crate-source "io-kit-sys" "0.4.1"
                "0ysy5k3wf54yangy25hkj10xx332cj2hb937xasg6riziv7yczk1"))

(define rust-is-executable-1.0.5
  (crate-source "is_executable" "1.0.5"
                "1i78ss45h94nwabbn6ki64a91djlli8zdwwbh56jj9kvhssbiaxs"))

(define rust-is-terminal-polyfill-1.70.1
  (crate-source "is_terminal_polyfill" "1.70.1"
                "1kwfgglh91z33kl0w5i338mfpa3zs0hidq5j4ny4rmjwrikchhvr"))

(define rust-itertools-0.12.1
  (crate-source "itertools" "0.12.1"
                "0s95jbb3ndj1lvfxyq5wanc0fm0r6hg6q4ngb92qlfdxvci10ads"))

(define rust-itoa-1.0.15
  (crate-source "itoa" "1.0.15"
                "0b4fj9kz54dr3wam0vprjwgygvycyw8r0qwg7vp19ly8b2w16psa"))

(define rust-itoa-1.0.9
  (crate-source "itoa" "1.0.9"
                "0f6cpb4yqzhkrhhg6kqsw3wnmmhdnnffi6r2xzy248gzi2v0l5dg"))

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

(define rust-libc-0.2.161
  (crate-source "libc" "0.2.161"
                "1lc5s3zd0491x9zxrv2kvclai1my1spz950pkkyry4vwh318k54f"))

(define rust-libc-0.2.176
  (crate-source "libc" "0.2.176"
                "0x7ivn80h7nz2l46vra7bxx36s6r8d0lkax14dx97skjsss2kyaq"))

(define rust-libdav-0.9.1
  (crate-source "libdav" "0.9.1"
                "1646mcnalav3jiprn3xyslyncmcvn34jzw5qn0h4k1x0bppczqhm"))

(define rust-libfuzzer-sys-0.4.10
  ;; TODO: Check bundled sources.
  (crate-source "libfuzzer-sys" "0.4.10"
                "0124z86582vyzl8gbadqscjgf9i94jcpa9mxcpsyxjvh3w71jdsh"))

(define rust-libgit2-sys-0.18.2+1.9.1
  ;; TODO: Check bundled sources.
  (crate-source "libgit2-sys" "0.18.2+1.9.1"
                "08n223x2pkf4gj6yrjmh3z6q236qj6nifwww78xcblrbvw1zwhhw"))

(define rust-libm-0.2.15
  (crate-source "libm" "0.2.15"
                "1plpzf0p829viazdj57yw5dhmlr8ywf3apayxc2f2bq5a6mvryzr"))

(define rust-libredox-0.1.10
  (crate-source "libredox" "0.1.10"
                "1jswil4ai90s4rh91fg8580x8nikni1zl3wnch4h01nvidqpwvs1"))

(define rust-libsqlite3-sys-0.30.1
  ;; TODO: Check bundled sources.
  (crate-source "libsqlite3-sys" "0.30.1"
                "0jcikvgbj84xc7ikdmpc8m4y5lyqgrb9aqblphwk67kv95xgp69f"))

(define rust-libz-sys-1.1.22
  ;; TODO: Check bundled sources.
  (crate-source "libz-sys" "1.1.22"
                "07b5wxh0ska996kc0g2hanjhmb4di7ksm6ndljhr4pi0vykyfw4b"))

(define rust-linked-hash-map-0.5.6
  (crate-source "linked-hash-map" "0.5.6"
                "03vpgw7x507g524nx5i1jf5dl8k3kv0fzg8v3ip6qqwbpkqww5q7"))

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

(define rust-litemap-0.8.0
  (crate-source "litemap" "0.8.0"
                "0mlrlskwwhirxk3wsz9psh6nxcy491n0dh8zl02qgj0jzpssw7i4"))

(define rust-litrs-0.4.2
  (crate-source "litrs" "0.4.2"
                "1v8bxsrkm0w2k9nmbp8hsspy9i1lawajywqdw4hx87rjzqv41rgm"))

(define rust-lock-api-0.4.10
  (crate-source "lock_api" "0.4.10"
                "05nd9nzxqidg24d1k8y5vlc8lz9gscpskrikycib46qbl8brgk61"))

(define rust-lock-api-0.4.13
  (crate-source "lock_api" "0.4.13"
                "0rd73p4299mjwl4hhlfj9qr88v3r0kc8s1nszkfmnq2ky43nb4wn"))

(define rust-log-0.4.20
  (crate-source "log" "0.4.20"
                "13rf7wphnwd61vazpxr7fiycin6cb1g8fmvgqg18i464p0y1drmm"))

(define rust-log-0.4.28
  (crate-source "log" "0.4.28"
                "0cklpzrpxafbaq1nyxarhnmcw9z3xcjrad3ch55mmr58xw2ha21l"))

(define rust-loop9-0.1.5
  (crate-source "loop9" "0.1.5"
                "0qphc1c0cbbx43pwm6isnwzwbg6nsxjh7jah04n1sg5h4p0qgbhg"))

(define rust-lru-0.16.1
  (crate-source "lru" "0.16.1"
                "1j3ffpdz734s75ish396165gv4yx1zihp6isif9vpyj6kwc4ksdz"))

(define rust-mach2-0.4.3
  (crate-source "mach2" "0.4.3"
                "0i6vcnbq5v51whgyidzhf7cbxqrmj2nkw8z0m2ib02rc60mjhh6n"))

(define rust-maybe-rayon-0.1.1
  (crate-source "maybe-rayon" "0.1.1"
                "06cmvhj4n36459g327ng5dnj8d58qs472pv5ahlhm7ynxl6g78cf"))

(define rust-memchr-2.6.2
  (crate-source "memchr" "2.6.2"
                "0vnqrsfm5260gcxyb83ipfd68d51l3azpm81i8dyc6320b8ax1jl"))

(define rust-memchr-2.7.6
  (crate-source "memchr" "2.7.6"
                "0wy29kf6pb4fbhfksjbs05jy2f32r2f3r1ga6qkmpz31k79h0azm"))

(define rust-memmap2-0.9.8
  (crate-source "memmap2" "0.9.8"
                "1dqxjs89krh8cin0k7ksqc9myw9yni9kn8d8cllwq4fn1isrhfl4"))

(define rust-memoffset-0.7.1
  (crate-source "memoffset" "0.7.1"
                "1x2zv8hv9c9bvgmhsjvr9bymqwyxvgbca12cm8xkhpyy5k1r7s2x"))

(define rust-mime-0.3.17
  (crate-source "mime" "0.3.17"
                "16hkibgvb9klh0w0jk5crr5xv90l3wlf77ggymzjmvl1818vnxv8"))

(define rust-minimad-0.13.1
  (crate-source "minimad" "0.13.1"
                "1lj5szpri8hjf38qrmg0i7vabp9b1rwakm5nly86a63d484dgid9"))

(define rust-minimal-lexical-0.2.1
  (crate-source "minimal-lexical" "0.2.1"
                "16ppc5g84aijpri4jzv14rvcnslvlpphbszc7zzp6vfkddf4qdb8"))

(define rust-miniz-oxide-0.7.1
  (crate-source "miniz_oxide" "0.7.1"
                "1ivl3rbbdm53bzscrd01g60l46lz5krl270487d8lhjvwl5hx0g7"))

(define rust-miniz-oxide-0.8.9
  (crate-source "miniz_oxide" "0.8.9"
                "05k3pdg8bjjzayq3rf0qhpirq9k37pxnasfn4arbs17phqn6m9qz"))

(define rust-mio-1.0.2
  (crate-source "mio" "1.0.2"
                "1v1cnnn44awxbcfm4zlavwgkvbyg7gp5zzjm8mqf1apkrwflvq40"))

(define rust-mio-1.0.4
  (crate-source "mio" "1.0.4"
                "073n3kam3nz8j8had35fd2nn7j6a33pi3y5w3kq608cari2d9gkq"))

(define rust-moka-0.12.8
  (crate-source "moka" "0.12.8"
                "0vrbsd86bdnliwgnzwqw6gi3x7n4fl8gnck4wzfx4xfr9pmn5krj"))

(define rust-moxcms-0.7.5
  (crate-source "moxcms" "0.7.5"
                "026df3qpxn430dlngpj3gjip0m9280g3asvbia5dpsjsjfl2zlyx"))

(define rust-mutate-once-0.1.2
  (crate-source "mutate_once" "0.1.2"
                "1byj4yz6h0mdx3mmj0mq23ma59kw41pckspr2gz8rl22k0y27lhk"))

(define rust-new-debug-unreachable-1.0.6
  (crate-source "new_debug_unreachable" "1.0.6"
                "11phpf1mjxq6khk91yzcbd3ympm78m3ivl7xg6lg2c0lf66fy3k5"))

(define rust-nix-0.26.4
  (crate-source "nix" "0.26.4"
                "06xgl4ybb8pvjrbmc3xggbgk3kbs1j0c4c0nzdfrmpbgrkrym2sr"))

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

(define rust-notify-8.2.0
  (crate-source "notify" "8.2.0"
                "1hrb83451vm5cpjw83nz5skgwjg5ara28zq8nxsqbzsif690fgad"))

(define rust-notify-types-2.0.0
  (crate-source "notify-types" "2.0.0"
                "0pcjm3wnvb7pvzw6mn89csv64ip0xhx857kr8jic5vddi6ljc22y"))

(define rust-num-bigint-0.4.6
  (crate-source "num-bigint" "0.4.6"
                "1f903zd33i6hkjpsgwhqwi2wffnvkxbn6rv4mkgcjcqi7xr4zr55"))

(define rust-num-conv-0.1.0
  (crate-source "num-conv" "0.1.0"
                "1ndiyg82q73783jq18isi71a7mjh56wxrk52rlvyx0mi5z9ibmai"))

(define rust-num-derive-0.4.2
  (crate-source "num-derive" "0.4.2"
                "00p2am9ma8jgd2v6xpsz621wc7wbn1yqi71g15gc3h67m7qmafgd"))

(define rust-num-integer-0.1.46
  (crate-source "num-integer" "0.1.46"
                "13w5g54a9184cqlbsq80rnxw4jj4s0d8wv75jsq5r2lms8gncsbr"))

(define rust-num-rational-0.4.2
  (crate-source "num-rational" "0.4.2"
                "093qndy02817vpgcqjnj139im3jl7vkq4h68kykdqqh577d18ggq"))

(define rust-num-traits-0.2.19
  (crate-source "num-traits" "0.2.19"
                "0h984rhdkkqd4ny9cif7y2azl3xdfb7768hb9irhpsch4q3gq787"))

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

(define rust-octseq-0.5.2
  (crate-source "octseq" "0.5.2"
                "04pycbrcxlmhxqmrs4jgd0kqjk9pwjil6zr4fp2wwi4wgjikqv0j"))

(define rust-once-cell-1.18.0
  (crate-source "once_cell" "1.18.0"
                "0vapcd5ambwck95wyz3ymlim35jirgnqn9a0qmi19msymv95v2yx"))

(define rust-once-cell-1.21.3
  (crate-source "once_cell" "1.21.3"
                "0b9x77lb9f1j6nqgf5aka4s2qj0nly176bpbrv6f9iakk5ff3xa2"))

(define rust-once-cell-polyfill-1.70.1
  (crate-source "once_cell_polyfill" "1.70.1"
                "1bg0w99srq8h4mkl68l1mza2n2f2hvrg0n8vfa3izjr5nism32d4"))

(define rust-open-1.7.1
  (crate-source "open" "1.7.1"
                "00828zcxdy3r38inz48jgnszgvqgi1a3bi2rrhij86mqsqq7msnw"))

(define rust-opener-0.8.3
  (crate-source "opener" "0.8.3"
                "0isfar4r3h25kf1z35mz8r1sdh8gilm3a51akp4007mr5ab2946b"))

(define rust-openssl-probe-0.1.5
  (crate-source "openssl-probe" "0.1.5"
                "1kq18qm48rvkwgcggfkqq6pm948190czqc94d6bm2sir5hq1l0gz"))

(define rust-option-ext-0.2.0
  (crate-source "option-ext" "0.2.0"
                "0zbf7cx8ib99frnlanpyikm1bx8qn8x602sw1n7bg6p9x94lyx04"))

(define rust-parking-2.2.1
  (crate-source "parking" "2.2.1"
                "1fnfgmzkfpjd69v4j9x737b1k8pnn054bvzcn5dm3pkgq595d3gk"))

(define rust-parking-lot-0.12.1
  (crate-source "parking_lot" "0.12.1"
                "13r2xk7mnxfc5g0g6dkdxqdqad99j7s7z8zhzz4npw5r0g0v4hip"))

(define rust-parking-lot-0.12.4
  (crate-source "parking_lot" "0.12.4"
                "04sab1c7304jg8k0d5b2pxbj1fvgzcf69l3n2mfpkdb96vs8pmbh"))

(define rust-parking-lot-core-0.9.11
  (crate-source "parking_lot_core" "0.9.11"
                "19g4d6m5k4ggacinqprnn8xvdaszc3y5smsmbz1adcdmaqm8v0xw"))

(define rust-parking-lot-core-0.9.8
  (crate-source "parking_lot_core" "0.9.8"
                "0ixlak319bpzldq20yvyfqk0y1vi736zxbw101jvzjp7by30rw4k"))

(define rust-paste-1.0.15
  (crate-source "paste" "1.0.15"
                "02pxffpdqkapy292harq6asfjvadgp1s005fip9ljfsn9fvxgh2p"))

(define rust-pathdiff-0.2.3
  (crate-source "pathdiff" "0.2.3"
                "1lrqp4ip05df8dzldq6gb2c1sq2gs54gly8lcnv3rhav1qhwx56z"))

(define rust-percent-encoding-2.3.2
  (crate-source "percent-encoding" "2.3.2"
                "083jv1ai930azvawz2khv7w73xh8mnylk7i578cifndjn5y64kwv"))

(define rust-phf-0.13.1
  (crate-source "phf" "0.13.1"
                "1pzswx5gdglgjgp4azyzwyr4gh031r0kcnpqq6jblga72z3jsmn1"))

(define rust-phf-generator-0.13.1
  (crate-source "phf_generator" "0.13.1"
                "0dwpp11l41dy9mag4phkyyvhpf66lwbp79q3ik44wmhyfqxcwnhk"))

(define rust-phf-macros-0.13.1
  (crate-source "phf_macros" "0.13.1"
                "1vv9h8pr7xh18sigpvq1hxc8q9nmjmv6gdpqsp65krxiahmh6bw1"))

(define rust-phf-shared-0.13.1
  (crate-source "phf_shared" "0.13.1"
                "0rpjchnswm0x5l4mz9xqfpw0j4w68sjvyqrdrv13h7lqqmmyyzz5"))

(define rust-pico-args-0.5.0
  (crate-source "pico-args" "0.5.0"
                "05d30pvxd6zlnkg2i3ilr5a70v3f3z2in18m67z25vinmykngqav"))

(define rust-pin-project-1.1.5
  (crate-source "pin-project" "1.1.5"
                "1cxl146x0q7lawp0m1826wsgj8mmmfs6ja8q7m6f7ff5j6vl7gxn"))

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

(define rust-pkg-config-0.3.29
  (crate-source "pkg-config" "0.3.29"
                "1jy6158v1316khkpmq2sjj1vgbnbnw51wffx7p0k0l9h9vlys019"))

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

(define rust-potential-utf-0.1.3
  (crate-source "potential_utf" "0.1.3"
                "12mhwvhpvvim6xqp6ifgkh1sniv9j2cmid6axn10fnjvpsnikpw4"))

(define rust-powerfmt-0.2.0
  (crate-source "powerfmt" "0.2.0"
                "14ckj2xdpkhv3h6l5sdmb9f1d57z8hbfpdldjc2vl5givq2y77j3"))

(define rust-ppv-lite86-0.2.17
  (crate-source "ppv-lite86" "0.2.17"
                "1pp6g52aw970adv3x2310n7glqnji96z0a9wiamzw89ibf0ayh2v"))

(define rust-ppv-lite86-0.2.21
  (crate-source "ppv-lite86" "0.2.21"
                "1abxx6qz5qnd43br1dd9b2savpihzjza8gb4fbzdql1gxp2f7sl5"))

(define rust-proc-macro2-1.0.101
  (crate-source "proc-macro2" "1.0.101"
                "1pijhychkpl7rcyf1h7mfk6gjfii1ywf5n0snmnqs5g4hvyl7bl9"))

(define rust-proc-macro2-1.0.86
  (crate-source "proc-macro2" "1.0.86"
                "0xrv22p8lqlfdf1w0pj4si8n2ws4aw0kilmziwf0vpv5ys6rwway"))

(define rust-proc-status-0.1.1
  (crate-source "proc-status" "0.1.1"
                "04lp8kdj75m8s1hwxslyzz3fdgbs6zy4zfjhg2s7cysyj6nc1q7h"))

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

(define rust-quick-error-2.0.1
  (crate-source "quick-error" "2.0.1"
                "18z6r2rcjvvf8cn92xjhm2qc3jpd1ljvcbf12zv0k9p565gmb4x9"))

(define rust-quick-xml-0.38.3
  (crate-source "quick-xml" "0.38.3"
                "12bvsbnnmlnq9xg9in3h3080ag3sisafgpcn7lqyzhkz93kk58j2"))

(define rust-quote-1.0.36
  (crate-source "quote" "1.0.36"
                "19xcmh445bg6simirnnd4fvkmp6v2qiwxh5f6rw4a70h76pnm9qg"))

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

(define rust-rayon-1.11.0
  (crate-source "rayon" "1.11.0"
                "13x5fxb7rn4j2yw0cr26n7782jkc7rjzmdkg42qxk3xz0p8033rn"))

(define rust-rayon-core-1.13.0
  (crate-source "rayon-core" "1.13.0"
                "14dbr0sq83a6lf1rfjq5xdpk5r6zgzvmzs5j6110vlv2007qpq92"))

(define rust-redox-syscall-0.3.5
  (crate-source "redox_syscall" "0.3.5"
                "0acgiy2lc1m2vr8cr33l5s7k9wzby8dybyab1a9p753hcbr68xjn"))

(define rust-redox-syscall-0.5.17
  (crate-source "redox_syscall" "0.5.17"
                "0xrvpchkaxph3r5ww2i04v9nwg3843fp3prf8kqlh1gv01b4c1sl"))

(define rust-redox-users-0.4.6
  (crate-source "redox_users" "0.4.6"
                "0hya2cxx6hxmjfxzv9n8rjl5igpychav7zfi1f81pz6i4krry05s"))

(define rust-regex-1.11.3
  (crate-source "regex" "1.11.3"
                "0b58ya98c4i5cjjiwhpcnjr61cv9g143qhdwhsryggj09098hllb"))

(define rust-regex-automata-0.4.11
  (crate-source "regex-automata" "0.4.11"
                "1bawj908pxixpggcnma3xazw53mwyz68lv9hn4yg63nlhv7bjgl3"))

(define rust-regex-syntax-0.8.6
  (crate-source "regex-syntax" "0.8.6"
                "00chjpglclfskmc919fj5aq308ffbrmcn7kzbkz92k231xdsmx6a"))

(define rust-resvg-0.45.1
  (crate-source "resvg" "0.45.1"
                "0hyz89wyasn0wdnpw7qy5iyl4xv3yx36hk3crb4h6pm5q2c8g4m8"))

(define rust-rgb-0.8.52
  (crate-source "rgb" "0.8.52"
                "1km115a9lblf9pldvx51dmmg30y8ms4ka67hvas2ndcq556qhshc"))

(define rust-ring-0.17.14
  (crate-source "ring" "0.17.14"
                "1dw32gv19ccq4hsx3ribhpdzri1vnrlcfqb2vj41xn4l49n9ws54"))

(define rust-roff-0.2.2
  (crate-source "roff" "0.2.2"
                "1wyqz6m0pm4p6wzhwhahvcidfm7nwb38zl4q7ha940pn3w66dy48"))

(define rust-roxmltree-0.20.0
  (crate-source "roxmltree" "0.20.0"
                "15vw91ps91wkmmgy62khf9zb63bdinvm80957dascbsw7dwvc83c"))

(define rust-rusqlite-0.32.1
  (crate-source "rusqlite" "0.32.1"
                "0vlx040bppl414pbjgbp7qr4jdxwszi9krx0m63zzf2f2whvflvp"))

(define rust-rust-fuzzy-search-0.1.1
  (crate-source "rust-fuzzy-search" "0.1.1"
                "1chvl47hq42r219yxs6r1dp4l19acy5ay145hpc5drgzaiq6amx1"))

(define rust-rustc-demangle-0.1.23
  (crate-source "rustc-demangle" "0.1.23"
                "0xnbk2bmyzshacjm2g1kd4zzv2y2az14bw3sjccq5qkpmsfvn9nn"))

(define rust-rustc-hash-2.1.1
  (crate-source "rustc-hash" "2.1.1"
                "03gz5lvd9ghcwsal022cgkq67dmimcgdjghfb5yb5d352ga06xrm"))

(define rust-rustc-version-0.4.1
  (crate-source "rustc_version" "0.4.1"
                "14lvdsmr5si5qbqzrajgb6vfn69k0sfygrvfvr2mps26xwi3mjyg"))

(define rust-rustix-0.38.10
  (crate-source "rustix" "0.38.10"
                "0r096k86c9sbvirfnz5vy8k8qphkkwahcvi6irqfn9d6rbhlhqpd"))

(define rust-rustix-0.38.44
  (crate-source "rustix" "0.38.44"
                "0m61v0h15lf5rrnbjhcb9306bgqrhskrqv7i1n0939dsw8dbrdgx"))

(define rust-rustix-1.1.2
  (crate-source "rustix" "1.1.2"
                "0gpz343xfzx16x82s1x336n0kr49j02cvhgxdvaq86jmqnigh5fd"))

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

(define rust-rustversion-1.0.22
  (crate-source "rustversion" "1.0.22"
                "0vfl70jhv72scd9rfqgr2n11m5i9l1acnk684m2w83w0zbqdx75k"))

(define rust-rustybuzz-0.20.1
  (crate-source "rustybuzz" "0.20.1"
                "00hp1gwykjfli258zs7lqg8p2zdh94dv2mw8zx7f73m0z2b7qg7x"))

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

(define rust-scopeguard-1.2.0
  (crate-source "scopeguard" "1.2.0"
                "0jcz9sd47zlsgcnm1hdw0664krxwb5gczlif4qngj2aif8vky54l"))

(define rust-secular-1.0.1
  (crate-source "secular" "1.0.1"
                "1davw8k29sycm7f4674d4m44jfa7pn812jm3m3mm76srvz63xp63"))

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

(define rust-serde-1.0.228
  (crate-source "serde" "1.0.228"
                "17mf4hhjxv5m90g42wmlbc61hdhlm6j9hwfkpcnd72rpgzm993ls"))

(define rust-serde-core-1.0.228
  (crate-source "serde_core" "1.0.228"
                "1bb7id2xwx8izq50098s5j2sqrrvk31jbbrjqygyan6ask3qbls1"))

(define rust-serde-derive-1.0.188
  (crate-source "serde_derive" "1.0.188"
                "1wjaclvsfxgqnnnykllvb5gffsxynk66x6h4c1ds6anq8b37mjjf"))

(define rust-serde-derive-1.0.228
  (crate-source "serde_derive" "1.0.228"
                "0y8xm7fvmr2kjcd029g9fijpndh8csv5m20g4bd76w8qschg4h6m"))

(define rust-serde-json-1.0.145
  (crate-source "serde_json" "1.0.145"
                "1767y6kxjf7gwpbv8bkhgwc50nhg46mqwm9gy9n122f7v1k6yaj0"))

(define rust-serde-spanned-0.6.9
  (crate-source "serde_spanned" "0.6.9"
                "18vmxq6qfrm110caszxrzibjhy2s54n1g5w1bshxq9kjmz7y0hdz"))

(define rust-serde-spanned-1.0.2
  (crate-source "serde_spanned" "1.0.2"
                "1vh4kcnzhw0fbr1jhg41p8yybnp5gmpnh171fy25bgn2a8s7h5sl"))

(define rust-sha2-0.10.7
  (crate-source "sha2" "0.10.7"
                "1n3flx8bjyblmb2n860g8402z7q10caajp2n403n37i3cbcbk7s7"))

(define rust-shell-words-1.1.0
  (crate-source "shell-words" "1.1.0"
                "1plgwx8r0h5ismbbp6cp03740wmzgzhip85k5hxqrrkaddkql614"))

(define rust-shlex-1.3.0
  (crate-source "shlex" "1.3.0"
                "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg"))

(define rust-signal-hook-0.3.18
  (crate-source "signal-hook" "0.3.18"
                "1qnnbq4g2vixfmlv28i1whkr0hikrf1bsc4xjy2aasj2yina30fq"))

(define rust-signal-hook-mio-0.2.4
  (crate-source "signal-hook-mio" "0.2.4"
                "1k8pl9aafiadr4czsg8zal9b4jdk6kq5985p90i19jc5sh31mnrl"))

(define rust-signal-hook-registry-1.4.1
  (crate-source "signal-hook-registry" "1.4.1"
                "18crkkw5k82bvcx088xlf5g4n3772m24qhzgfan80nda7d3rn8nq"))

(define rust-signal-hook-registry-1.4.6
  (crate-source "signal-hook-registry" "1.4.6"
                "12y2v1ms5z111fymaw1v8k93m5chnkp21h0jknrydkj8zydp395j"))

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

(define rust-siphasher-1.0.1
  (crate-source "siphasher" "1.0.1"
                "17f35782ma3fn6sh21c027kjmd227xyrx06ffi8gw4xzv9yry6an"))

(define rust-slab-0.4.9
  (crate-source "slab" "0.4.9"
                "0rxvsgir0qw5lkycrqgb1cxsvxzjv9bmx73bk5y42svnzfba94lg"))

(define rust-slotmap-1.0.7
  (crate-source "slotmap" "1.0.7"
                "0amqb2fn9lcy1ri0risblkcp88dl0rnfmynw7lx0nqwza77lmzyv"))

(define rust-smallvec-1.13.2
  (crate-source "smallvec" "1.13.2"
                "0rsw5samawl3wsw6glrsb127rx6sh89a8wyikicw6dkdcjd1lpiw"))

(define rust-smallvec-1.15.1
  (crate-source "smallvec" "1.15.1"
                "00xxdxxpgyq5vjnpljvkmy99xij5rxgh913ii1v16kzynnivgcb7"))

(define rust-snafu-0.7.5
  (crate-source "snafu" "0.7.5"
                "1mj2j2gfbf8mm1hr02zrbrqrh2zp01f61xgkx0lpln2w0ankgpp4"))

(define rust-snafu-derive-0.7.5
  (crate-source "snafu-derive" "0.7.5"
                "1gzy9rzggs090zf7hfvgp4lm1glrmg9qzh796686jnq7bxk7j04r"))

(define rust-socket2-0.5.7
  (crate-source "socket2" "0.5.7"
                "070r941wbq76xpy039an4pyiy3rfj7mp7pvibf1rcri9njq5wc6f"))

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

(define rust-str-buf-1.0.6
  (crate-source "str-buf" "1.0.6"
                "1l7q4nha7wpsr0970bfqm773vhmpwr9l6rr8r4gwgrh46wvdh24y"))

(define rust-strict-0.2.0
  (crate-source "strict" "0.2.0"
                "01j0h28xzg07kd1km5m0wz88asp6hwh45n8q8bdkjymqlpz4897l"))

(define rust-strict-num-0.1.1
  (crate-source "strict-num" "0.1.1"
                "0cb7l1vhb8zj90mzm8avlk815k40sql9515s865rqdrdfavvldv6"))

(define rust-strsim-0.11.1
  (crate-source "strsim" "0.11.1"
                "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))

(define rust-subtle-2.5.0
  (crate-source "subtle" "2.5.0"
                "1g2yjs7gffgmdvkkq0wrrh0pxds3q0dv6dhkw9cdpbib656xdkc1"))

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

(define rust-syn-2.0.67
  (crate-source "syn" "2.0.67"
                "144c0apb6zqqlxdyiiy42ynlahn1ddw66cpxwd7azww63pnmb1pz"))

(define rust-synstructure-0.13.2
  (crate-source "synstructure" "0.13.2"
                "1lh9lx3r3jb18f8sbj29am5hm9jymvbwh6jb1izsnnxgvgrp12kj"))

(define rust-syntect-no-panic-6.0.0
  (crate-source "syntect-no-panic" "6.0.0"
                "1im47kg8jdc70v4xn43wkiz2kl9mgjjznrgnwanspsaxryx8z96m"))

(define rust-system-deps-6.2.2
  (crate-source "system-deps" "6.2.2"
                "0j93ryw031n3h8b0nfpj5xwh3ify636xmv8kxianvlyyipmkbrd3"))

(define rust-tagptr-0.2.0
  (crate-source "tagptr" "0.2.0"
                "05r4mwvlsclx1ayj65hpzjv3dn4wpi8j4xm695vydccf9k7r683v"))

(define rust-target-lexicon-0.12.16
  (crate-source "target-lexicon" "0.12.16"
                "1cg3bnx1gdkdr5hac1hzxy64fhw4g7dqkd0n3dxy5lfngpr1mi31"))

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

(define rust-termux-clipboard-0.1.0
  (crate-source "termux-clipboard" "0.1.0"
                "0vz3b2c0ra7x5wg6i56247j8z58wkv3dknznjidk34rjr89zyslz"))

(define rust-thiserror-1.0.47
  (crate-source "thiserror" "1.0.47"
                "13wdsrdyrq6x3rcydvxlx4mxck0c5v3mz1dj8zp7xhdg63n05a4p"))

(define rust-thiserror-1.0.69
  (crate-source "thiserror" "1.0.69"
                "0lizjay08agcr5hs9yfzzj6axs53a2rgx070a1dsi3jpkcrzbamn"))

(define rust-thiserror-2.0.17
  (crate-source "thiserror" "2.0.17"
                "1j2gixhm2c3s6g96vd0b01v0i0qz1101vfmw0032mdqj1z58fdgn"))

(define rust-thiserror-impl-1.0.47
  (crate-source "thiserror-impl" "1.0.47"
                "16z1irxb45l011af53diap97x44dixnbp60v9g6pvarrdssj7dkb"))

(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))

(define rust-thiserror-impl-2.0.17
  (crate-source "thiserror-impl" "2.0.17"
                "04y92yjwg1a4piwk9nayzjfs07sps8c4vq9jnsfq9qvxrn75rw9z"))

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

(define rust-tinystr-0.8.1
  (crate-source "tinystr" "0.8.1"
                "12sc6h3hnn6x78iycm5v6wrs2xhxph0ydm43yyn7gdfw8l8nsksx"))

(define rust-tinyvec-1.10.0
  (crate-source "tinyvec" "1.10.0"
                "1yhk0qdqyiaa4v2j9h8pzax5gxgwpz4da0lcphfil6g6pk1zv9dz"))

(define rust-tinyvec-macros-0.1.1
  (crate-source "tinyvec_macros" "0.1.1"
                "081gag86208sc3y6sdkshgw3vysm5d34p431dzw0bshz66ncng0z"))

(define rust-tokio-1.41.0
  (crate-source "tokio" "1.41.0"
                "1fwb4nm630hmy9cyl2ar6wxqckgvsakwhg1rhjza4is3a09k8pql"))

(define rust-tokio-macros-2.4.0
  (crate-source "tokio-macros" "2.4.0"
                "0lnpg14h1v3fh2jvnc8cz7cjf0m7z1xgkwfpcyy632g829imjgb9"))

(define rust-tokio-rustls-0.25.0
  (crate-source "tokio-rustls" "0.25.0"
                "03w6d5aqqf084rmcmrsyq5grhydl53blaiqcl0i2yfnv187hqpkp"))

(define rust-toml-0.8.23
  (crate-source "toml" "0.8.23"
                "0qnkrq4lm2sdhp3l6cb6f26i8zbnhqb7mhbmksd550wxdfcyn6yw"))

(define rust-toml-0.9.7
  (crate-source "toml" "0.9.7"
                "187av4nsjc0cdfixpc24sqpxqwy5ijvdm7hd9yfsqx94pzcybr80"))

(define rust-toml-datetime-0.6.11
  (crate-source "toml_datetime" "0.6.11"
                "077ix2hb1dcya49hmi1avalwbixmrs75zgzb3b2i7g2gizwdmk92"))

(define rust-toml-datetime-0.7.2
  (crate-source "toml_datetime" "0.7.2"
                "1hgff8gdk9yx7dljkqfijmj0sc5ln4xhpj045divdhi7xifhiw9j"))

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

(define rust-tracing-core-0.1.32
  (crate-source "tracing-core" "0.1.32"
                "0m5aglin3cdwxpvbg6kz0r9r0k31j48n0kcfwsp6l49z26k3svf0"))

(define rust-tracing-core-0.1.34
  (crate-source "tracing-core" "0.1.34"
                "0y3nc4mpnr79rzkrcylv5f5bnjjp19lsxwis9l4kzs97ya0jbldr"))

(define rust-trash-5.2.3
  (crate-source "trash" "5.2.3"
                "1wsgyriw10wl4h46pgs589laq4vwh8q5ma4aflwpl08j212k98v5"))

(define rust-triomphe-0.1.11
  (crate-source "triomphe" "0.1.11"
                "1crf71hndy3fc68x8v4aikkdjynp4n5sdhq28sck8x7frx8bd7l5"))

(define rust-try-lock-0.2.4
  (crate-source "try-lock" "0.2.4"
                "1vc15paa4zi06ixsxihwbvfn24d708nsyg1ncgqwcrn42byyqa1m"))

(define rust-ttf-parser-0.25.1
  (crate-source "ttf-parser" "0.25.1"
                "0cbgqglcwwjg3hirwq6xlza54w04mb5x02kf7zx4hrw50xmr1pyj"))

(define rust-typenum-1.16.0
  (crate-source "typenum" "1.16.0"
                "1fhb9iaqyjn4dzn2vl86kxjhp4xpw5gynczlnqzf4x6rjgpn2ya9"))

(define rust-umask-2.1.0
  (crate-source "umask" "2.1.0"
                "071xszsd6znk0ik11pxl7mwhf07clsiq3qpzw1ac0dcyak14d6pc"))

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

(define rust-unicode-width-0.2.1
  (crate-source "unicode-width" "0.2.1"
                "0k0mlq7xy1y1kq6cgv1r2rs2knn6rln3g3af50rhi0dkgp60f6ja"))

(define rust-untrusted-0.9.0
  (crate-source "untrusted" "0.9.0"
                "1ha7ib98vkc538x0z60gfn0fc5whqdd85mb87dvisdcaifi6vjwf"))

(define rust-url-2.5.7
  (crate-source "url" "2.5.7"
                "0nzghdv0kcksyvri0npxbjzyx2ihprks5k590y77bld355m17g08"))

(define rust-urlencoding-2.1.3
  (crate-source "urlencoding" "2.1.3"
                "1nj99jp37k47n0hvaz5fvz7z6jd0sb4ppvfy3nphr1zbnyixpy6s"))

(define rust-usvg-0.45.1
  (crate-source "usvg" "0.45.1"
                "1vs7gfhyxjgkgibgwfjniwrszfw0iivj1aq06hq8nfxfzc39pgl0"))

(define rust-utf8-iter-1.0.4
  (crate-source "utf8_iter" "1.0.4"
                "1gmna9flnj8dbyd8ba17zigrp9c4c3zclngf5lnb5yvz1ri41hdn"))

(define rust-utf8parse-0.2.2
  (crate-source "utf8parse" "0.2.2"
                "088807qwjq46azicqwbhlmzwrbkz7l4hpw43sdkdyyk524vdxaq6"))

(define rust-uuid-1.11.0
  (crate-source "uuid" "1.11.0"
                "0sj4l28lif2wm4xrafdfgqjywjzv43wzp8nii9a4i539myhg1igq"))

(define rust-uzers-0.12.1
  (crate-source "uzers" "0.12.1"
                "1pcpi9v90nr3q2y3i4pkac9c20r1nzaimvcm7vajmn770ksizy2d"))

(define rust-v-frame-0.3.9
  (crate-source "v_frame" "0.3.9"
                "1qkvb4ks33zck931vzqckjn36hkngj6l2cwmvfsnlpc7r0kpfsv6"))

(define rust-vcpkg-0.2.15
  (crate-source "vcpkg" "0.2.15"
                "09i4nf5y8lig6xgj3f7fyrvzd3nlaw4znrihw8psidvv5yk4xkdc"))

(define rust-version-check-0.9.4
  (crate-source "version_check" "0.9.4"
                "0gs8grwdlgh0xq660d7wr80x14vxbizmd8dbp29p2pdncx8lp1s9"))

(define rust-version-check-0.9.5
  (crate-source "version_check" "0.9.5"
                "0nhhi4i5x89gm911azqbn7avs9mdacw2i3vcz3cnmz3mv4rqz4hb"))

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

(define rust-wasm-bindgen-backend-0.2.104
  (crate-source "wasm-bindgen-backend" "0.2.104"
                "069vnhhn2j4w2gwd8rch6g8d3iwkrgi45fas6i3qm7glcrd9l737"))

(define rust-wasm-bindgen-backend-0.2.95
  (crate-source "wasm-bindgen-backend" "0.2.95"
                "0n53wgy78bgzgjwk0z69zbspzhv8p2a4zh69s4fzvpqdrb9x8vfb"))

(define rust-wasm-bindgen-macro-0.2.104
  (crate-source "wasm-bindgen-macro" "0.2.104"
                "06d1m5bg272h6jabq0snm7c50fifjz6r20f5hqlmz7y5wivh99kw"))

(define rust-wasm-bindgen-macro-0.2.95
  (crate-source "wasm-bindgen-macro" "0.2.95"
                "0mic8b2vab1a91m6x3hjxkwz23094bq1cwhnszarsnlggyz894z7"))

(define rust-wasm-bindgen-macro-support-0.2.104
  (crate-source "wasm-bindgen-macro-support" "0.2.104"
                "1mr18kx7ima1pmsqlkk982q4a0vf3r8s1x6901jb59sd1prd41wz"))

(define rust-wasm-bindgen-macro-support-0.2.95
  (crate-source "wasm-bindgen-macro-support" "0.2.95"
                "0s7g6glb85lyx2pj83shbmg4d50mvqhb2c2qk2j28yigaxbspii6"))

(define rust-wasm-bindgen-shared-0.2.104
  (crate-source "wasm-bindgen-shared" "0.2.104"
                "1la1xj9v3gmawnlyi7lc3mb3xi447r6frb98hi2fb9m1nb47vmms"))

(define rust-wasm-bindgen-shared-0.2.95
  (crate-source "wasm-bindgen-shared" "0.2.95"
                "1386q7mvv5ky003hcc6yyxpid3y1m7fy0l920i3z3ab60vqhkz35"))

(define rust-web-sys-0.3.72
  ;; TODO: Check bundled sources.
  (crate-source "web-sys" "0.3.72"
                "04k19hilj9r8sx6q20fz853149gfpmf83yk2zvq0s14c2288nj7n"))

(define rust-weezl-0.1.10
  (crate-source "weezl" "0.1.10"
                "1wqnxqn8n90bgazs6djlibf58ppdxki4slblwp9lgnq0fwkv6ld7"))

(define rust-which-4.4.2
  (crate-source "which" "4.4.2"
                "1ixzmx3svsv5hbdvd8vdhd3qwvf6ns8jdpif1wmwsy10k90j9fl7"))

(define rust-winapi-0.3.9
  (crate-source "winapi" "0.3.9"
                "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))

(define rust-winapi-i686-pc-windows-gnu-0.4.0
  (crate-source "winapi-i686-pc-windows-gnu" "0.4.0"
                "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))

(define rust-winapi-util-0.1.11
  (crate-source "winapi-util" "0.1.11"
                "08hdl7mkll7pz8whg869h58c1r9y7in0w0pk8fm24qc77k0b39y2"))

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

(define rust-winnow-0.7.13
  (crate-source "winnow" "0.7.13"
                "1krrjc1wj2vx0r57m9nwnlc1zrhga3fq41d8w9hysvvqb5mj7811"))

(define rust-wit-bindgen-0.46.0
  (crate-source "wit-bindgen" "0.46.0"
                "0ngysw50gp2wrrfxbwgp6dhw1g6sckknsn3wm7l00vaf7n48aypi"))

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

(define rust-xmlwriter-0.1.0
  (crate-source "xmlwriter" "0.1.0"
                "1fg0ldmkgiis6hnxpi1c9gy7v23y0lpi824bp8yp12fi3r82lypc"))

(define rust-xterm-query-0.5.2
  (crate-source "xterm-query" "0.5.2"
                "0cak14nd70qd99vrrikzaqlks6k8l7gyrbx7hz6lxpjg8ggk6b19"))

(define rust-yaml-rust-0.4.5
  (crate-source "yaml-rust" "0.4.5"
                "118wbqrr4n6wgk5rjjnlrdlahawlxc1bdsx146mwk8f79in97han"))

(define rust-yoke-0.8.0
  (crate-source "yoke" "0.8.0"
                "1k4mfr48vgi7wh066y11b7v1ilakghlnlhw9snzz8vi2p00vnhaz"))

(define rust-yoke-derive-0.8.0
  (crate-source "yoke-derive" "0.8.0"
                "1dha5jrjz9jaq8kmxq1aag86b98zbnm9lyjrihy5sv716sbkrniq"))

(define rust-zerocopy-0.8.27
  (crate-source "zerocopy" "0.8.27"
                "0b1870gf2zzlckca69v2k4mqwmf8yh2li37qldnzvvd3by58g508"))

(define rust-zerocopy-derive-0.8.27
  (crate-source "zerocopy-derive" "0.8.27"
                "0c9qrylm2p55dvaplxsl24ma48add9qk4y0d6kjbkllaqvcvill8"))

(define rust-zerofrom-0.1.6
  (crate-source "zerofrom" "0.1.6"
                "19dyky67zkjichsb7ykhv0aqws3q0jfvzww76l66c19y6gh45k2h"))

(define rust-zerofrom-derive-0.1.6
  (crate-source "zerofrom-derive" "0.1.6"
                "00l5niw7c1b0lf1vhvajpjmcnbdp2vn96jg4nmkhq2db0rp5s7np"))

(define rust-zeroize-1.7.0
  (crate-source "zeroize" "1.7.0"
                "0bfvby7k9pdp6623p98yz2irqnamcyzpn7zh20nqmdn68b0lwnsj"))

(define rust-zerotrie-0.2.2
  (crate-source "zerotrie" "0.2.2"
                "15gmka7vw5k0d24s0vxgymr2j6zn2iwl12wpmpnpjgsqg3abpw1n"))

(define rust-zerovec-0.11.4
  (crate-source "zerovec" "0.11.4"
                "0fz7j1ns8d86m2fqg2a4bzi5gnh5892bxv4kcr9apwc6a3ajpap7"))

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
