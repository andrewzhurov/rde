(define-module (rde packages aws-sam-cli2)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system go)
  #:use-module (guix build-system haskell)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system ocaml)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system r)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system trivial)
  #:use-module (guix deprecation)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages code)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages java)
  #:use-module (gnu packages java-compression)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages jupyter)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages node)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages skribilo)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages uglifyjs)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix platform)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (gnu packages node))

;; May need all the deps listed in https://github.com/Delgan/loguru/runs/7939769060?check_suite_focus=true

(define-public python-loguru
  (package
    (name "python-loguru")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "loguru" version))
              (sha256
               (base32
                "076l16ilgdb0pjbbkx21d39kzysvlyswdnbghgli79fhb1kx0sq6"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (invoke "tox" "-e" "tests") ;; found in "Run tests" phase here https://github.com/Delgan/loguru/runs/7939769060?check_suite_focus=true
             )))))
    (native-inputs
     (list python-pytest python-tox python-colorama))
    (home-page "https://github.com/Delgan/loguru")
    (synopsis "")
    (description
     "")
    (license license:bsd-2)))

;; loguru ; n

(define-public python-utils
  (package
    (name "python-utils")
    (version "3.3.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "python-utils" version))
              (sha256
               (base32
                "13a7cibdaqxqamazm6p9rrka3008fgm75yn5xs015sa0drq8n71v"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (delete-file "pytest.ini")
             (invoke "pytest" "-vv"))))))
    (native-inputs
     `(("python-loguru" ,python-loguru)
       ("pytest-runner" ,python-pytest-runner)
       ("pytest" ,python-pytest)
       ("six" ,python-six)))
    (home-page "https://github.com/WoLpH/python-utils")
    (synopsis "Convenient utilities not included with the standard Python install")
    (description
      "Python Utils is a collection of small Python functions and classes which
     make common patterns shorter and easier.")
    (license license:bsd-2)))

;; utils ; outdated?

(define-public python-pyelftools
  (package
    (name "python-pyelftools")
    (version "7e53b5a1a98377328bf37008bd53aa9d19c2d24d")

    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/eliben/pyelftools")
             (commit "7e53b5a1a98377328bf37008bd53aa9d19c2d24d")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fb28s8ncx18az1bl7ykpx4h3bag8as2zn34cg8bz4fksrp182yx"))))
    (build-system python-build-system)
    (native-inputs
     (list python-utils
           ))
    ;; (arguments
    ;;  (list
    ;;   #:phases
    ;;   #~(modify-phases %standard-phases
    ;;       (replace 'check
    ;;         (lambda* (#:key tests? #:allow-other-keys)
    ;;           (if tests?
    ;;               (begin
    ;;                 (invoke "python" "./test_spec.py"))
    ;;               (format #t "test suite not run~%"))))
    ;;       )))
    (home-page "https://github.com/eliben/pyelftools")
    (synopsis "")
    (description
     "")
    (license license:expat))) ;; fix to custom

;; elftools ; n ;; pyelftools I guess

(define-public python-aws-lambda-builders
  (package
    (name "python-aws-lambda-builders")
    (version "1.19.0")

    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/aws-lambda-builders")
             (commit "826e4ef7bd26f6a1e64feb11553ccb79201d4168")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f5xfsimcxvycrb8m48k53bd3c38spbbjq5rrppw2fzxizdzyrlr"))))

    ;; No tests in PyPI archive
    ;; (source
    ;;  (origin
    ;;    (method url-fetch)
    ;;    (uri (pypi-uri "aws-lambda-builders" version))
    ;;    (sha256
    ;;     (base32 "1gqfh00ics2k1sm5g46l3bi8cl5fc5d1cwzh1ylvcxvdvypklqc7"))))
    (build-system python-build-system)
    (native-inputs
     (list python-wheel python-pytest python-parameterized python-mock python-numpy
           node ;; Trying to solve "Unable to find a workflow matching given capability: nodejs, npm-esbuild, None"
           ))
    ;; (arguments
    ;;  (list
    ;;   #:phases
    ;;   #~(modify-phases %standard-phases
    ;;       (replace 'check
    ;;         (lambda* (#:key tests? #:allow-other-keys)
    ;;           (if tests?
    ;;               (begin
    ;;                 (invoke "python" "./test_spec.py"))
    ;;               (format #t "test suite not run~%"))))
    ;;       )))
    (home-page "https://github.com/aws/aws-lambda-builders")
    (synopsis "")
    (description
     "")
    (license license:expat))) ;; fix to apache

(define-public python-chevron
  (package
    (name "python-chevron")
    (version "march-21-2021")

    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/noahmorrison/chevron")
             (commit "5e1c12827b7fc3db30cb3b24cae9a7ee3092822b")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1v0b4snlqn0knpyyaljwpdml5skg3gly2lk8rqlf49w9b29331z3"))))

    ;; No tests in PyPI archive
    ;; (source
    ;;  (origin
    ;;    (method url-fetch)
    ;;    (uri (pypi-uri "chevron" version))
    ;;    (sha256
    ;;     (base32 "1gqfh00ics2k1sm5g46l3bi8cl5fc5d1cwzh1ylvcxvdvypklqc7"))))
    (build-system python-build-system)
    (native-inputs
     (list python-coveralls python-tox))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (if tests?
                  (begin
                    (invoke "python" "./test_spec.py"))
                  (format #t "test suite not run~%"))))
          )))
    (home-page "https://github.com/noahmorrison/chevron")
    (synopsis "")
    (description
     "")
    (license license:expat)))

;; pipenv n

;; (define-public python-serverlessrepo
;;   (package
;;     (name "python-serverlessrepo")
;;     (version "aug-17-2019")

;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/amazon-archives/aws-serverlessrepo-python")
;;              (commit "b08cb944d93519e05087fa51993a778623b69ec0")))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32 "1ka69j1nrl07xys1khl8j6g9shvygg0b9c77hqgikfjz3rcn78mv"))))

;;     ;; No tests in PyPI archive
;;     ;; (source
;;     ;;  (origin
;;     ;;    (method url-fetch)
;;     ;;    (uri (pypi-uri "serverlessrepo" version))
;;     ;;    (sha256
;;     ;;     (base32 "1gqfh00ics2k1sm5g46l3bi8cl5fc5d1cwzh1ylvcxvdvypklqc7"))))
;;     (build-system python-build-system)
;;     (native-inputs
;;      (list python-pip))
;;     (arguments
;;      (list
;;       #:phases
;;       #~(modify-phases %standard-phases
;;           (replace 'check
;;             (lambda* (#:key tests? #:allow-other-keys)
;;               (if tests?
;;                   (begin
;;                     (invoke #$(file-append coreutils "/bin/ls") "-l")
;;                     (invoke #$(file-append coreutils "/bin/cat") "Makefile")
;;                     (invoke "pipenv")
;;                     (invoke "make" "build")
;;                     (invoke "make" "test"))
;;                   (format #t "test suite not run~%"))))
;;           )))
;;     (home-page "https://github.com/amazon-archives/aws-serverlessrepo-python")
;;     (synopsis "")
;;     (description
;;      "")
;;     (license license:expat)))


;; An error occurred while installing alabaster==0.7.12 --hash=sha256:446438bdcca0e05bd45ea2de1668c1d9b032e1a9154c2c259092d77031ddd359 --hash=sha256:a661d72d58e6ea8a57f7a86e37d86716863ee5e92788398526d58b26a4e4dc02! Will try again.
;; An error occurred while installing arpeggio==1.10.2 --hash=sha256:bfe349f252f82f82d84cb886f1d5081d1a31451e6045275e9f90b65d0daa06f1 --hash=sha256:fed68a1cb7f529cbd4d725597cc811b7506885fcdef17d4cdcf564341a1e210b! Will try again.
;; An error occurred while installing atomicwrites==1.4.1; sys_platform == 'win32' --hash=sha256:81b2c9071a49367a7f770170e5eec8cb66567cfbbc8c73d20ce5ca4a8d71cf11! Will try again.
;; An error occurred while installing attrs==22.1.0; python_version >= '3.5' --hash=sha256:29adc2665447e5191d0e7c568fde78b21f9672d344281d0c6e1ab085429b22b6 --hash=sha256:86efa402f67bf2df34f51a335487cf46b1ec130d02b8d39fd248abfd30da551c! Will try again.
;; An error occurred while installing babel==2.10.3; python_version >= '3.6' --hash=sha256:ff56f4892c1c4bf0d814575ea23471c230d544203c7748e8c68f0089478d48eb --hash=sha256:7614553711ee97490f732126dc077f8d0ae084ebc6a96e23db1482afabdb2c51! Will try again.
;; An error occurred while installing beautifulsoup4==4.11.1; python_version >= '3.6' --hash=sha256:ad9aa55b65ef2808eb405f46cf74df7fcb7044d5cbc26487f96eb2ef2e436693 --hash=sha256:58d5c3d29f5a36ffeb94f02f0d786cd53014cf9b3b3951d42e0080d8a9498d30! Will try again.
;; An error occurred while installing black==22.8.0; python_full_version >= '3.6.2' --hash=sha256:0ad827325a3a634bae88ae7747db1a395d5ee02cf05d9aa7a9bd77dfb10e940c --hash=sha256:0a12e4e1353819af41df998b02c6742643cfef58282915f781d0e4dd7a200411 --hash=sha256:cea1b2542d4e2c02c332e83150e41e3ca80dc0fb8de20df3c5e98e242156222c --hash=sha256:53198e28a1fb865e9fe97f88220da2e44df6da82b18833b588b1883b16bb5d41 --hash=sha256:bc4d4123830a2d190e9cc42a2e43570f82ace35c3aeb26a512a2102bce5af7ec --hash=sha256:ce957f1d6b78a8a231b18e0dd2d94a33d2ba738cd88a7fe64f53f659eea49fdd --hash=sha256:d839150f61d09e7217f52917259831fe2b689f5c8e5e32611736351b89bb2a90 --hash=sha256:3b2c25f8dea5e8444bdc6788a2f543e1fb01494e144480bc17f806178378005e --hash=sha256:8ce13ffed7e66dda0da3e0b2eb1bdfc83f5812f66e09aca2b0978593ed636b6c --hash=sha256:d2c21d439b2baf7aa80d6dd4e3659259be64c6f49dfd0f32091063db0e006db4 --hash=sha256:78dd85caaab7c3153054756b9fe8c611efa63d9e7aecfa33e533060cb14b6d16 --hash=sha256:792f7eb540ba9a17e8656538701d3eb1afcb134e3b45b71f20b25c77a8db7e6e --hash=sha256:e981e20ec152dfb3e77418fb616077937378b322d7b26aa1ff87717fb18b4875 --hash=sha256:5594efbdc35426e35a7defa1ea1a1cb97c7dbd34c0e49af7fb593a36bd45edab --hash=sha256:4a098a69a02596e1f2a58a2a1c8d5a05d5a74461af552b371e82f9fa4ada8342 --hash=sha256:dd82842bb272297503cbec1a2600b6bfb338dae017186f8f215c8958f8acf869 --hash=sha256:5107ea36b2b61917956d018bd25129baf9ad1125e39324a9b18248d362156a27 --hash=sha256:32a4b17f644fc288c6ee2bafdf5e3b045f4eff84693ac069d87b1a347d861497 --hash=sha256:c3a73f66b6d5ba7288cd5d6dad9b4c9b43f4e8a4b789a94bf5abfb878c663eb3 --hash=sha256:5b879eb439094751185d1cfdca43023bc6786bd3c60372462b6f051efa6281a5 --hash=sha256:a983526af1bea1e4cf6768e649990f28ee4f4137266921c2c3cee8116ae42ec3 --hash=sha256:e8166b7bfe5dcb56d325385bd1d1e0f635f24aae14b3ae437102dedc0c186747 --hash=sha256:a05da0430bd5ced89176db098567973be52ce175a55677436a271102d7eaa3fe! Will try again.
;; An error occurred while installing bs4==0.0.1 --hash=sha256:36ecea1fd7cc5c0c6e4a1ff075df26d50da647b75376626cc186e2212886dd3a! Will try again.
;; An error occurred while installing certifi==2022.6.15; python_version >= '3.6' --hash=sha256:fe86415d55e84719d75f8b69414f6438ac3547d2078ab91b67e779ef69378412 --hash=sha256:84c85a9078b11105f04f3036a9482ae10e4621616db313fe045dd24743a0820d! Will try again.
;; An error occurred while installing cfgv==3.3.1; python_full_version >= '3.6.1' --hash=sha256:f5a830efb9ce7a445376bb66ec94c638a9787422f96264c98edc6bdeed8ab736 --hash=sha256:c6a0883f3917a037485059700b9e75da2464e6c27051014ad85ba6aaa5884426! Will try again.
;; An error occurred while installing charset-normalizer==2.1.1; python_version >= '3.6' --hash=sha256:83e9a75d1911279afd89352c68b45348559d1fc0506b054b346651b5e7fee29f --hash=sha256:5a3d016c7c547f69d6f81fb0db9449ce888b418b5b9952cc5e6e66843e9dd845! Will try again.
;; An error occurred while installing click==8.0.3 --hash=sha256:353f466495adaeb40b6b5f592f9f91cb22372351c84caeb068132442a4518ef3 --hash=sha256:410e932b050f5eed773c4cda94de75971c89cdb3155a72a0831139a79e5ecb5b! Will try again.
;; An error occurred while installing click-default-group==1.2.2 --hash=sha256:d9560e8e8dfa44b3562fbc9425042a0fd6d21956fcc2db0077f63f34253ab904! Will try again.
;; An error occurred while installing colorama==0.4.5; platform_system == 'Windows' --hash=sha256:854bf444933e37f5824ae7bfc1e98d5bce2ebe4160d46b5edf346a89358e99da --hash=sha256:e6c6b4334fc50988a639d9b98aa429a0b57da6e17b9a44f0451f930b6967b7a4! Will try again.
;; An error occurred while installing coverage[toml]==6.4.4; python_version >= '3.7' --hash=sha256:9c7b9b498eb0c0d48b4c2abc0e10c2d78912203f972e0e63e3c9dc21f15abdaa --hash=sha256:ee6ae6bbcac0786807295e9687169fba80cb0617852b2fa118a99667e8e6815d --hash=sha256:35ef1f8d8a7a275aa7410d2f2c60fa6443f4a64fae9be671ec0696a68525b875 --hash=sha256:66e6df3ac4659a435677d8cd40e8eb1ac7219345d27c41145991ee9bf4b806a0 --hash=sha256:98c0b9e9b572893cdb0a00e66cf961a238f8d870d4e1dc8e679eb8bdc2eb1b86 --hash=sha256:67f9346aeebea54e845d29b487eb38ec95f2ecf3558a3cffb26ee3f0dcc3e760 --hash=sha256:ee2b2fb6eb4ace35805f434e0f6409444e1466a47f620d1d5763a22600f0f892 --hash=sha256:15e38d853ee224e92ccc9a851457fb1e1f12d7a5df5ae44544ce7863691c7a0d --hash=sha256:e1fabd473566fce2cf18ea41171d92814e4ef1495e04471786cbc943b89a3781 --hash=sha256:42c499c14efd858b98c4e03595bf914089b98400d30789511577aa44607a1b74 --hash=sha256:c1328d0c2f194ffda30a45f11058c02410e679456276bfa0bbe0b0ee87225fac --hash=sha256:354df19fefd03b9a13132fa6643527ef7905712109d9c1c1903f2133d3a4e145 --hash=sha256:cf2afe83a53f77aec067033199797832617890e15bed42f4a1a93ea24794ae3e --hash=sha256:e16c45b726acb780e1e6f88b286d3c10b3914ab03438f32117c4aa52d7f30d58 --hash=sha256:6113e4df2fa73b80f77663445be6d567913fb3b82a86ceb64e44ae0e4b695de1 --hash=sha256:08002f9251f51afdcc5e3adf5d5d66bb490ae893d9e21359b085f0e03390a820 --hash=sha256:7a98d6bf6d4ca5c07a600c7b4e0c5350cd483c85c736c522b786be90ea5bac4f --hash=sha256:e7b4da9bafad21ea45a714d3ea6f3e1679099e420c8741c74905b92ee9bfa7cc --hash=sha256:5f444627b3664b80d078c05fe6a850dd711beeb90d26731f11d492dcbadb6973 --hash=sha256:cbbb0e4cd8ddcd5ef47641cfac97d8473ab6b132dd9a46bacb18872828031685 --hash=sha256:ff934ced84054b9018665ca3967fc48e1ac99e811f6cc99ea65978e1d384454b --hash=sha256:4b7101938584d67e6f45f0015b60e24a95bf8dea19836b1709a80342e01b472f --hash=sha256:fc600f6ec19b273da1d85817eda339fb46ce9eef3e89f220055d8696e0a06908 --hash=sha256:c35cca192ba700979d20ac43024a82b9b32a60da2f983bec6c0f5b84aead635c --hash=sha256:cdbb0d89923c80dbd435b9cf8bba0ff55585a3cdb28cbec65f376c041472c60d --hash=sha256:ef6f44409ab02e202b31a05dd6666797f9de2aa2b4b3534e9d450e42dea5e817 --hash=sha256:14a32ec68d721c3d714d9b105c7acf8e0f8a4f4734c811eda75ff3718570b5e3 --hash=sha256:6913dddee2deff8ab2512639c5168c3e80b3ebb0f818fed22048ee46f735351a --hash=sha256:8d032bfc562a52318ae05047a6eb801ff31ccee172dc0d2504614e911d8fa83e --hash=sha256:e431e305a1f3126477abe9a184624a85308da8edf8486a863601d58419d26ffa --hash=sha256:fcbe3d9a53e013f8ab88734d7e517eb2cd06b7e689bedf22c0eb68db5e4a0a19 --hash=sha256:d5dd4b8e9cd0deb60e6fcc7b0647cbc1da6c33b9e786f9c79721fd303994832f --hash=sha256:564cd0f5b5470094df06fab676c6d77547abfdcb09b6c29c8a97c41ad03b103c --hash=sha256:dfa0b97eb904255e2ab24166071b27408f1f69c8fbda58e9c0972804851e0558 --hash=sha256:f855b39e4f75abd0dfbcf74a82e84ae3fc260d523fcb3532786bcbbcb158322c --hash=sha256:9d6e1f3185cbfd3d91ac77ea065d85d5215d3dfa45b191d14ddfcd952fa53796 --hash=sha256:6a864733b22d3081749450466ac80698fe39c91cb6849b2ef8752fd7482011f3 --hash=sha256:61b993f3998ee384935ee423c3d40894e93277f12482f6e777642a0141f55782 --hash=sha256:4179502f210ebed3ccfe2f78bf8e2d59e50b297b598b100d6c6e3341053066a2 --hash=sha256:a3b2752de32c455f2521a51bd3ffb53c5b3ae92736afde67ce83477f5c1dd928 --hash=sha256:e3d3c4cc38b2882f9a15bafd30aec079582b819bec1b8afdbde8f7797008108a --hash=sha256:01778769097dbd705a24e221f42be885c544bb91251747a8a3efdec6eb4788f2 --hash=sha256:f67cf9f406cf0d2f08a3515ce2db5b82625a7257f88aad87904674def6ddaec1 --hash=sha256:7026f5afe0d1a933685d8f2169d7c2d2e624f6255fb584ca99ccca8c0e966fd7 --hash=sha256:783bc7c4ee524039ca13b6d9b4186a67f8e63d91342c713e88c1865a38d0892a --hash=sha256:9cc4f107009bca5a81caef2fca843dbec4215c05e917a59dec0c8db5cff1d2aa --hash=sha256:1238b08f3576201ebf41f7c20bf59baa0d05da941b123c6656e42cdb668e9827 --hash=sha256:a095aa0a996ea08b10580908e88fbaf81ecf798e923bbe64fb98d1807db3d68a --hash=sha256:ab066f5ab67059d1f1000b5e1aa8bbd75b6ed1fc0014559aea41a9eb66fc2ce0 --hash=sha256:fde17bc42e0716c94bf19d92e4c9f5a00c5feb401f5bc01101fdf2a8b7cacf60! Will try again.
;; An error occurred while installing distlib==0.3.6 --hash=sha256:14bad2d9b04d3a36127ac97f30b12a19268f211063d8f8ee4f47108896e11b46 --hash=sha256:f35c4b692542ca110de7ef0bea44d73981caeb34ca0b9b6b2e6d7790dda8f80e! Will try again.
;; An error occurred while installing docutils==0.17.1; python_version >= '2.7' and python_version not in '3.0, 3.1, 3.2, 3.3, 3.4' --hash=sha256:cf316c8370a737a022b72b56874f6602acf974a37a9fba42ec2876387549fc61 --hash=sha256:686577d2e4c32380bb50cbb22f575ed742d58168cee37e99117a854bcd88f125! Will try again.
;; An error occurred while installing execnet==1.9.0; python_version >= '2.7' and python_version not in '3.0, 3.1, 3.2, 3.3, 3.4' --hash=sha256:8f694f3ba9cc92cab508b152dcfe322153975c29bda272e2fd7f3f00f36e47c5 --hash=sha256:a295f7cc774947aac58dde7fdc85f4aa00c42adf5d8f5468fc630c1acf30a142! Will try again.
;; An error occurred while installing filelock==3.8.0; python_version >= '3.7' --hash=sha256:55447caa666f2198c5b6b13a26d2084d26fa5b115c00d065664b2124680c4edc --hash=sha256:617eb4e5eedc82fc5f47b6d61e4d11cb837c56cb4544e39081099fa17ad109d4! Will try again.
;; An error occurred while installing flake8==3.9.2; python_version >= '2.7' and python_version not in '3.0, 3.1, 3.2, 3.3, 3.4' --hash=sha256:07528381786f2a6237b061f6e96610a4167b226cb926e2aa2b6b1d78057c576b --hash=sha256:bf8fd333346d844f616e8d47905ef3a3384edae6b4e9beb0c5101e25e3110907! Will try again.
;; An error occurred while installing flaky==3.7.0; python_version >= '2.7' and python_version not in '3.0, 3.1, 3.2, 3.3' --hash=sha256:3ad100780721a1911f57a165809b7ea265a7863305acb66708220820caf8aa0d --hash=sha256:d6eda73cab5ae7364504b7c44670f70abed9e75f77dd116352f662817592ec9c! Will try again.
;; An error occurred while installing flask==2.2.2; python_version >= '3.7' --hash=sha256:642c450d19c4ad482f96729bd2a8f6d32554aa1e231f4f6b4e7e5264b16cca2b --hash=sha256:b9c46cc36662a7949f34b52d8ec7bb59c0d74ba08ba6cb9ce9adc1d8676d9526! Will try again.
;; An error occurred while installing gunicorn==20.1.0; sys_platform == 'linux' --hash=sha256:e0a968b5ba15f8a328fdfd7ab1fcb5af4470c28aaf7e55df02a99bc13138e6e8 --hash=sha256:9dcc4547dbb1cb284accfb15ab5667a0e5d1881cc443e0677b4882a4067a807e! Will try again.
;; An error occurred while installing identify==2.5.3; python_version >= '3.7' --hash=sha256:25851c8c1370effb22aaa3c987b30449e9ff0cece408f810ae6ce408fdd20893 --hash=sha256:887e7b91a1be152b0d46bbf072130235a8117392b9f1828446079a816a05ef44! Will try again.
;; An error occurred while installing idna==3.3; python_version >= '3.5' --hash=sha256:84d9dd047ffa80596e0f246e2eab0b391788b0503584e8945f2368256d2735ff --hash=sha256:9d643ff0a55b762d5cdb124b8eaa99c66322e2157b69160bc32796e824360e6d! Will try again.
;; An error occurred while installing imagesize==1.4.1; python_version >= '2.7' and python_version not in '3.0, 3.1, 3.2, 3.3' --hash=sha256:0d8d18d08f840c19d0ee7ca1fd82490fdc3729b7ac93f49870406ddde8ef8d8b --hash=sha256:69150444affb9cb0d5cc5a92b3676f0b2fb7cd9ae39e947a5e11a36b4497cd4a! Will try again.
;; An error occurred while installing importlib-metadata==4.12.0; python_version >= '3.7' --hash=sha256:637245b8bab2b6502fcbc752cc4b7a6f6243bb02b31c5c26156ad103d3d45670 --hash=sha256:7401a975809ea1fdc658c3aa4f78cc2195a0e019c5cbc4c06122884e9ae80c23! Will try again.
;; An error occurred while installing incremental==21.3.0 --hash=sha256:02f5de5aff48f6b9f665d99d48bfc7ec03b6e3943210de7cfc88856d755d6f57 --hash=sha256:92014aebc6a20b78a8084cdd5645eeaa7f74b8933f70fa3ada2cfbd1e3b54321! Will try again.
;; An error occurred while installing iniconfig==1.1.1 --hash=sha256:bc3af051d7d14b2ee5ef9969666def0cd1a000e121eaea580d4a313df4b37f32 --hash=sha256:011e24c64b7f47f6ebd835bb12a743f2fbe9a26d4cecaa7f53bc4f35ee9da8b3! Will try again.
;; An error occurred while installing invoke==1.7.1 --hash=sha256:2dc975b4f92be0c0a174ad2d063010c8a1fdb5e9389d69871001118b4fcac4fb --hash=sha256:7b6deaf585eee0a848205d0b8c0014b9bf6f287a8eb798818a642dff1df14b19! Will try again.
;; An error occurred while installing itsdangerous==2.1.2; python_version >= '3.7' --hash=sha256:2c2349112351b88699d8d4b6b075022c0808887cb7ad10069318a8b0bc88db44 --hash=sha256:5dbbc68b317e5e42f327f9021763545dc3fc3bfe22e6deb96aaf1fc38874156a! Will try again.
;; An error occurred while installing jinja2==3.1.2; python_version >= '3.7' --hash=sha256:31351a702a408a9e7595a8fc6150fc3f43bb6bf7e319770cbc0db9df9437e852 --hash=sha256:6088930bfe239f0e6710546ab9c19c9ef35e29792895fed6e6e31a023a182a61! Will try again.
;; An error occurred while installing markupsafe==2.1.1; python_version >= '3.7' --hash=sha256:671cd1187ed5e62818414afe79ed29da836dde67166a9fac6d435873c44fdd02 --hash=sha256:e04e26803c9c3851c931eac40c695602c6295b8d432cbe78609649ad9bd2da8a --hash=sha256:86b1f75c4e7c2ac2ccdaec2b9022845dbb81880ca318bb7a0a01fbf7813e3812 --hash=sha256:8e576a51ad59e4bfaac456023a78f6b5e6e7651dcd383bcc3e18d06f9b55d6d1 --hash=sha256:fc7b548b17d238737688817ab67deebb30e8073c95749d55538ed473130ec0c7 --hash=sha256:694deca8d702d5db21ec83983ce0bb4b26a578e71fbdbd4fdcd387daa90e4d5e --hash=sha256:56442863ed2b06d19c37f94d999035e15ee982988920e12a5b4ba29b62ad1f77 --hash=sha256:d4306c36ca495956b6d568d276ac11fdd9c30a36f1b6eb928070dc5360b22e1c --hash=sha256:dda30ba7e87fbbb7eab1ec9f58678558fd9a6b8b853530e176eabd064da81417 --hash=sha256:f121a1420d4e173a5d96e47e9a0c0dcff965afdf1626d28de1460815f7c4ee7a --hash=sha256:b09bf97215625a311f669476f44b8b318b075847b49316d3e28c08e41a7a573f --hash=sha256:4b9fe39a2ccc108a4accc2676e77da025ce383c108593d65cc909add5c3bd601 --hash=sha256:6fbf47b5d3728c6aea2abb0589b5d30459e369baa772e0f37a0320185e87c980 --hash=sha256:3799351e2336dc91ea70b034983ee71cf2f9533cdff7c14c90ea126bfd95d65a --hash=sha256:e72591e9ecd94d7feb70c1cbd7be7b3ebea3f548870aa91e2732960fa4d57a37 --hash=sha256:99a2a507ed3ac881b975a2976d59f38c19386d128e7a9a18b7df6fff1fd4c1d6 --hash=sha256:a49907dd8420c5685cfa064a1335b6754b74541bbb3706c259c02ed65b644b3e --hash=sha256:efc1913fd2ca4f334418481c7e595c00aad186563bbc1ec76067848c7ca0a933 --hash=sha256:6a074d34ee7a5ce3effbc526b7083ec9731bb3cbf921bbe1d3005d4d2bdb3a63 --hash=sha256:d5ee4f386140395a2c818d149221149c54849dfcfcb9f1debfe07a8b8bd63f9a --hash=sha256:0212a68688482dc52b2d45013df70d169f542b7394fc744c02a57374a4207003 --hash=sha256:96e37a3dc86e80bf81758c152fe66dbf60ed5eca3d26305edf01892257049925 --hash=sha256:7f91197cc9e48f989d12e4e6fbc46495c446636dfc81b9ccf50bb0ec74b91d4b --hash=sha256:bcb3ed405ed3222f9904899563d6fc492ff75cce56cba05e32eff40e6acbeaa3 --hash=sha256:43093fb83d8343aac0b1baa75516da6092f58f41200907ef92448ecab8825135 --hash=sha256:e1c0b87e09fa55a220f058d1d49d3fb8df88fbfab58558f1198e08c1e1de842a --hash=sha256:10c1bfff05d95783da83491be968e8fe789263689c02724e0c691933c52994f5 --hash=sha256:8e3dcf21f367459434c18e71b2a9532d96547aef8a871872a5bd69a715c15f96 --hash=sha256:421be9fbf0ffe9ffd7a378aafebbf6f4602d564d34be190fc19a193232fd12b1 --hash=sha256:3ce11ee3f23f79dbd06fb3d63e2f6af7b12db1d46932fe7bd8afa259a5996603 --hash=sha256:089cf3dbf0cd6c100f02945abeb18484bd1ee57a079aefd52cffd17fba910b88 --hash=sha256:e8c843bbcda3a2f1e3c2ab25913c80a3c5376cd00c6e8c4a86a89a28c8dc5452 --hash=sha256:6d0072fea50feec76a4c418096652f2c3238eaa014b2f94aeb1d56a66b41403f --hash=sha256:b87db4360013327109564f0e591bd2a3b318547bcef31b468a92ee504d07ae4f --hash=sha256:97a68e6ada378df82bc9f16b800ab77cbf4b2fada0081794318520138c088e4a --hash=sha256:46d00d6cfecdde84d40e572d63735ef81423ad31184100411e6e3388d405e247 --hash=sha256:b7bd98b796e2b6553da7225aeb61f447f80a1ca64f41d83612e6139ca5213aa4 --hash=sha256:4a33dea2b688b3190ee12bd7cfa29d39c9ed176bda40bfa11099a3ce5d3a7ac6 --hash=sha256:8dc1c72a69aa7e082593c4a203dcf94ddb74bb5c8a731e4e1eb68d031e8498ff --hash=sha256:33b74d289bd2f5e527beadcaa3f401e0df0a89927c1559c8566c066fa4248ab7! Will try again.
;; An error occurred while installing mccabe==0.6.1 --hash=sha256:ab8a6258860da4b6677da4bd2fe5dc2c659cff31b3ee4f7f5d64e79735b80d42 --hash=sha256:dd8d182285a0fe56bace7f45b5e7d1a6ebcbf524e8f3bd87eb0f125271b8831f! Will try again.
;; An error occurred while installing mock==4.0.3; python_version >= '3.6' --hash=sha256:7d3fbbde18228f4ff2f1f119a45cdffa458b4c0dee32eb4d2bb2f82554bac7bc --hash=sha256:122fcb64ee37cfad5b3f48d7a7d51875d7031aaf3d8be7c42e2bee25044eee62! Will try again.
;; An error occurred while installing mypy-extensions==0.4.3 --hash=sha256:2d82818f5bb3e369420cb3c4060a7970edba416647068eb4c5343488a6c604a8 --hash=sha256:090fedd75945a69ae91ce1303b5824f428daf5a028d2f6ab8a299250a846f15d! Will try again.
;; An error occurred while installing nodeenv==1.7.0; python_version >= '2.7' and python_version not in '3.0, 3.1, 3.2, 3.3, 3.4, 3.5, 3.6' --hash=sha256:e0e7f7dfb85fc5394c6fe1e8fa98131a2473e04311a45afb6508f7cf1836fa2b --hash=sha256:27083a7b96a25f2f5e1d8cb4b6317ee8aeda3bdd121394e5ac54e498028a042e! Will try again.
;; An error occurred while installing packaging==21.3; python_version >= '3.6' --hash=sha256:ef103e05f519cdc783ae24ea4e2e0f508a9c99b2d4969652eed6a2e1ea5bd522 --hash=sha256:dd47c42927d89ab911e606518907cc2d3a1f38bbd026385970643f9c5b8ecfeb! Will try again.
;; An error occurred while installing parver==0.3.1; python_version >= '2.7' and python_version not in '3.0, 3.1, 3.2, 3.3' --hash=sha256:41a548c51b006a2f2522b54293cbfd2514bffa10774ece8430c9964a20cbd8b4 --hash=sha256:c902e0653bcce927cc156a7fd9b3a51924cbce3bf3d0bfd49fc282bfd0c5dfd3! Will try again.
;; An error occurred while installing pathspec==0.10.1; python_version >= '3.7' --hash=sha256:7ace6161b621d31e7902eb6b5ae148d12cfd23f4a249b9ffb6b9fee12084323d --hash=sha256:46846318467efc4556ccfd27816e004270a9eeeeb4d062ce5e6fc7a87c573f93! Will try again.
;; An error occurred while installing platformdirs==2.5.2; python_version >= '3.7' --hash=sha256:027d8e83a2d7de06bbac4e5ef7e023c02b863d7ea5d079477e722bb41ab25788 --hash=sha256:58c8abb07dcb441e6ee4b11d8df0ac856038f944ab98b7be6b27b2a3c7feef19! Will try again.
;; An error occurred while installing pluggy==1.0.0; python_version >= '3.6' --hash=sha256:74134bbf457f031a36d68416e1509f34bd5ccc019f0bcc952c7b909d06b37bd3 --hash=sha256:4224373bacce55f955a878bf9cfa763c1e360858e330072059e10bad68531159! Will try again.
;; An error occurred while installing pre-commit==2.20.0 --hash=sha256:51a5ba7c480ae8072ecdb6933df22d2f812dc897d5fe848778116129a681aac7 --hash=sha256:a978dac7bc9ec0bcee55c18a277d553b0f419d259dadb4b9418ff2d00eb43959! Will try again.
;; An error occurred while installing py==1.11.0; python_version >= '2.7' and python_version not in '3.0, 3.1, 3.2, 3.3, 3.4' --hash=sha256:51c75c4126074b472f746a24399ad32f6053d1b34b68d2fa41e558e6f4a98719 --hash=sha256:607c53218732647dff4acdfcd50cb62615cedf612e72d1724fb1a0cc6405b378! Will try again.
;; An error occurred while installing pycodestyle==2.7.0; python_version >= '2.7' and python_version not in '3.0, 3.1, 3.2, 3.3' --hash=sha256:c389c1d06bf7904078ca03399a4816f974a1d590090fecea0c63ec26ebaf1cef --hash=sha256:514f76d918fcc0b55c6680472f0a37970994e07bbb80725808c17089be302068! Will try again.
;; An error occurred while installing pyenchant==3.2.2; python_version >= '3.5' --hash=sha256:5a636832987eaf26efe971968f4d1b78e81f62bca2bde0a9da210c7de43c3bce --hash=sha256:5facc821ece957208a81423af7d6ec7810dad29697cb0d77aae81e4e11c8e5a6 --hash=sha256:1cf830c6614362a78aab78d50eaf7c6c93831369c52e1bb64ffae1df0341e637 --hash=sha256:6153f521852e23a5add923dbacfbf4bebbb8d70c4e4bad609a8e0f9faeb915d1! Will try again.
;; An error occurred while installing pyflakes==2.3.1; python_version >= '2.7' and python_version not in '3.0, 3.1, 3.2, 3.3' --hash=sha256:f5bc8ecabc05bb9d291eb5203d6810b49040f6ff446a756326104746cc00c1db --hash=sha256:7893783d01b8a89811dd72d7dfd4d84ff098e5eed95cfa8905b22bbffe52efc3! Will try again.
;; An error occurred while installing pygments==2.13.0; python_version >= '3.6' --hash=sha256:56a8508ae95f98e2b9bdf93a6be5ae3f7d8af858b43e02c5a2ff083726be40c1 --hash=sha256:f643f331ab57ba3c9d89212ee4a2dabc6e94f117cf4eefde99a0574720d14c42! Will try again.
;; An error occurred while installing pyparsing==3.0.9; python_full_version >= '3.6.8' --hash=sha256:2b020ecf7d21b687f219b71ecad3631f644a47f01403fa1d1036b0c6416d70fb --hash=sha256:5026bae9a10eeaefb61dab2f09052b9f4307d44aee4eda64b309723d8d206bbc! Will try again.
;; An error occurred while installing pypiserver==1.5.0 --hash=sha256:dc689cf5a7b2b3ea941766bbf48b7d85ad01e9babcf90e40897b6aae2ad9f98e --hash=sha256:fe2cbf08dfb8435767e9e8e2ea9aebf78f9382204c76d40f3bc79b45bb24f6eb! Will try again.
;; An error occurred while installing pytest==7.1.3; python_version >= '3.7' --hash=sha256:4f365fec2dff9c1162f834d9f18af1ba13062db0c708bf7b946f8a5c76180c39 --hash=sha256:1377bda3466d70b55e3f5cecfa55bb7cfcf219c7964629b967c37cf0bda818b7! Will try again.
;; An error occurred while installing pytest-cov==3.0.0 --hash=sha256:e7f0f5b1617d2210a2cabc266dfe2f4c75a8d32fb89eafb7ad9d06f6d076d470 --hash=sha256:578d5d15ac4a25e5f961c938b85a05b09fdaae9deef3bb6de9a6e766622ca7a6! Will try again.
;; An error occurred while installing pytest-forked==1.4.0; python_version >= '3.6' --hash=sha256:bbbb6717efc886b9d64537b41fb1497cfaf3c9601276be8da2cccfea5a3c8ad8 --hash=sha256:8b67587c8f98cbbadfdd804539ed5455b6ed03802203485dd2f53c1422d7440e! Will try again.
;; An error occurred while installing pytest-timeout==2.1.0; python_version >= '3.6' --hash=sha256:f6f50101443ce70ad325ceb4473c4255e9d74e3c7cd0ef827309dfa4c0d975c6 --hash=sha256:c07ca07404c612f8abbe22294b23c368e2e5104b521c1790195561f37e1ac3d9! Will try again.
;; An error occurred while installing pytest-xdist==2.5.0; python_version >= '3.6' --hash=sha256:6fe5c74fec98906deb8f2d2b616b5c782022744978e7bd4695d39c8f42d0ce65 --hash=sha256:4580deca3ff04ddb2ac53eba39d76cb5dd5edeac050cb6fbc768b0dd712b4edf! Will try again.
;; An error occurred while installing pytz==2022.2.1 --hash=sha256:cea221417204f2d1a2aa03ddae3e867921971d0d76f14d87abb4414415bbdcf5 --hash=sha256:220f481bdafa09c3955dfbdddb7b57780e9a94f5127e35456a48589b9e0c0197! Will try again.
;; An error occurred while installing pyyaml==6.0; python_version >= '3.6' --hash=sha256:0ce82d761c532fe4ec3f87fc45688bdd3a4c1dc5e0b4a19814b9009a29baefd4 --hash=sha256:d67d839ede4ed1b28a4e8909735fc992a923cdb84e618544973d7dfc71540803 --hash=sha256:daf496c58a8c52083df09b80c860005194014c3698698d1a57cbcfa182142a3a --hash=sha256:0b4624f379dab24d3725ffde76559cff63d9ec94e1736b556dacdfebe5ab6d4b --hash=sha256:c5687b8d43cf58545ade1fe3e055f70eac7a5a1a0bf42824308d868289a95737 --hash=sha256:a80a78046a72361de73f8f395f1f1e49f956c6be882eed58505a15f3e430962b --hash=sha256:cba8c411ef271aa037d7357a2bc8f9ee8b58b9965831d9e51baf703280dc73d3 --hash=sha256:277a0ef2981ca40581a47093e9e2d13b3f1fbbeffae064c1d21bfceba2030287 --hash=sha256:819b3830a1543db06c4d4b865e70ded25be52a2e0631ccd2f6a47a2822f2fd7c --hash=sha256:68fb519c14306fec9720a2a5b45bc9f0c8d1b9c72adf45c37baedfcd949c35a2 --hash=sha256:77f396e6ef4c73fdc33a9157446466f1cff553d979bd00ecb64385760c6babdc --hash=sha256:b3d267842bf12586ba6c734f89d1f5b871df0273157918b0ccefa29deb05c21c --hash=sha256:f84fbc98b019fef2ee9a1cb3ce93e3187a6df0b2538a651bfb890254ba9f90b5 --hash=sha256:d4eccecf9adf6fbcc6861a38015c2a64f38b9d94838ac1810a9023a0609e1b78 --hash=sha256:07751360502caac1c067a8132d150cf3d61339af5691fe9e87803040dbc5db57 --hash=sha256:897b80890765f037df3403d22bab41627ca8811ae55e9a722fd0392850ec4d86 --hash=sha256:9df7ed3b3d2e0ecfe09e14741b857df43adb5a3ddadc919a2d94fbdf78fea53c --hash=sha256:50602afada6d6cbfad699b0c7bb50d5ccffa7e46a3d738092afddc1f9758427f --hash=sha256:9fa600030013c4de8165339db93d182b9431076eb98eb40ee068700c9c813e34 --hash=sha256:b5b9eccad747aabaaffbc6064800670f0c297e52c12754eb1d976c57e4f74dcb --hash=sha256:d15a181d1ecd0d4270dc32edb46f7cb7733c7c508857278d3d378d14d606db2d --hash=sha256:d4db7c7aef085872ef65a8fd7d6d09a14ae91f691dec3e87ee5ee0539d516f53 --hash=sha256:213c60cd50106436cc818accf5baa1aba61c0189ff610f64f4a3e8c6726218ba --hash=sha256:48c346915c114f5fdb3ead70312bd042a953a8ce5c7106d5bfb1a5254e47da92 --hash=sha256:e61ceaab6f49fb8bdfaa0f92c4b57bcfbea54c09277b1b4f7ac376bfb7a7c174 --hash=sha256:1e4747bc279b4f613a09eb64bba2ba602d8a6664c6ce6396a4d0cd413a50ce07 --hash=sha256:2cd5df3de48857ed0544b34e2d40e9fac445930039f3cfe4bcc592a1f836d513 --hash=sha256:98c4d36e99714e55cfbaaee6dd5badbc9a1ec339ebfc3b1f52e293aee6bb71a4 --hash=sha256:40527857252b61eacd1d9af500c3337ba8deb8fc298940291486c465c8b46ec0 --hash=sha256:473f9edb243cb1935ab5a084eb238d842fb8f404ed2193a915d1784b5a6b5fc0 --hash=sha256:0283c35a6a9fbf047493e3a0ce8d79ef5030852c51e9d911a27badfde0605293 --hash=sha256:055d937d65826939cb044fc8c9b08889e8c743fdc6a32b33e2390f66013e449b --hash=sha256:231710d57adfd809ef5d34183b8ed1eeae3f76459c18fb4a0b373ad56bedcdd9! Will try again.
;; An error occurred while installing requests==2.28.1; python_version >= '3.7' and python_version < '4' --hash=sha256:8fefa2a1a1365bf5520aac41836fbee479da67864514bdb821f31ce07ce65349 --hash=sha256:7c5599b102feddaa661c826c56ab4fee28bfd17f5abca1ebbe3e7f19d7c97983! Will try again.
;; An error occurred while installing setuptools==65.3.0; python_version >= '3.7' --hash=sha256:2e24e0bec025f035a2e72cdd1961119f557d78ad331bb00ff82efb2ab8da8e82 --hash=sha256:7732871f4f7fa58fb6bdcaeadb0161b2bd046c85905dbaa066bdcbcc81953b57! Will try again.
;; An error occurred while installing six==1.16.0; python_version >= '2.7' and python_version not in '3.0, 3.1, 3.2, 3.3' --hash=sha256:1e61c37477a1626458e36f7b1d82aa5c9b094fa4802892072e49de9c60c4c926 --hash=sha256:8abb2f1d86890a2dfb989f9a77cfcfd3e47c2a354b01111771326f8aa26e0254! Will try again.
;; An error occurred while installing snowballstemmer==2.2.0 --hash=sha256:c8e1716e83cc398ae16824e5572ae04e0d9fc2c6b985fb0f900f5f0c96ecba1a --hash=sha256:09b16deb8547d3412ad7b590689584cd0fe25ec8db3be37788be3810cbf19cb1! Will try again.
;; An error occurred while installing soupsieve==2.3.2.post1; python_version >= '3.6' --hash=sha256:fc53893b3da2c33de295667a0e19f078c14bf86544af307354de5fcf12a3f30d --hash=sha256:3b2503d3c7084a42b1ebd08116e5f81aadfaea95863628c80a3b774a11b7c759! Will try again.
;; An error occurred while installing sphinx==4.5.0 --hash=sha256:ebf612653238bcc8f4359627a9b7ce44ede6fdd75d9d30f68255c7383d3a6226 --hash=sha256:7bf8ca9637a4ee15af412d1a1d9689fec70523a68ca9bb9127c2f3eeb344e2e6! Will try again.
;; An error occurred while installing sphinx-click==4.3.0 --hash=sha256:bd4db5d3c1bec345f07af07b8e28a76cfc5006d997984e38ae246bbf8b9a3b38 --hash=sha256:23e85a3cb0b728a421ea773699f6acadefae171d1a764a51dd8ec5981503ccbe! Will try again.
;; An error occurred while installing sphinxcontrib-applehelp==1.0.2; python_version >= '3.5' --hash=sha256:806111e5e962be97c29ec4c1e7fe277bfd19e9652fb1a4392105b43e01af885a --hash=sha256:a072735ec80e7675e3f432fcae8610ecf509c5f1869d17e2eecff44389cdbc58! Will try again.
;; An error occurred while installing sphinxcontrib-devhelp==1.0.2; python_version >= '3.5' --hash=sha256:8165223f9a335cc1af7ffe1ed31d2871f325254c0423bc0c4c7cd1c1e4734a2e --hash=sha256:ff7f1afa7b9642e7060379360a67e9c41e8f3121f2ce9164266f61b9f4b338e4! Will try again.
;; An error occurred while installing sphinxcontrib-htmlhelp==2.0.0; python_version >= '3.6' --hash=sha256:d412243dfb797ae3ec2b59eca0e52dac12e75a241bf0e4eb861e450d06c6ed07 --hash=sha256:f5f8bb2d0d629f398bf47d0d69c07bc13b65f75a81ad9e2f71a63d4b7a2f6db2! Will try again.
;; An error occurred while installing sphinxcontrib-jsmath==1.0.1; python_version >= '3.5' --hash=sha256:a9925e4a4587247ed2191a22df5f6970656cb8ca2bd6284309578f2153e0c4b8 --hash=sha256:2ec2eaebfb78f3f2078e73666b1415417a116cc848b72e5172e596c871103178! Will try again.
;; An error occurred while installing sphinxcontrib-qthelp==1.0.3; python_version >= '3.5' --hash=sha256:4c33767ee058b70dba89a6fc5c1892c0d57a54be67ddd3e7875a18d14cba5a72 --hash=sha256:bd9fc24bcb748a8d51fd4ecaade681350aa63009a347a8c14e637895444dfab6! Will try again.
;; An error occurred while installing sphinxcontrib-serializinghtml==1.1.5; python_version >= '3.5' --hash=sha256:352a9a00ae864471d3a7ead8d7d79f5fc0b57e8b3f95e9867eb9eb28999b92fd --hash=sha256:aa5f6de5dfdf809ef505c4895e51ef5c9eac17d0f287933eb49ec495280b6952! Will try again.
;; An error occurred while installing sphinxcontrib-spelling==7.6.0 --hash=sha256:6c1313618412511109f7b76029fbd60df5aa4acf67a2dc9cd1b1016d15e882ff --hash=sha256:292cd7e1f73a763451693b4d48c9bded151084f6a91e5337733e9fa8715d20ec! Will try again.
;; An error occurred while installing stdeb==0.10.0; sys_platform == 'linux' --hash=sha256:08c22c9c03b28a140fe3ec5064b53a5288279f22e596ca06b0be698d50c93cf2! Will try again.
;; An error occurred while installing toml==0.10.2; python_version >= '2.6' and python_version not in '3.0, 3.1, 3.2, 3.3' --hash=sha256:806143ae5bfb6a3c6e736a764057db0e6a0e05e338b5630894a5f779cabb4f9b --hash=sha256:b3bda1d108d5dd99f4a20d24d9c348e91c4db7ab1b749200bded2f839ccbe68f! Will try again.
;; An error occurred while installing tomli==2.0.1; python_full_version < '3.11.0a7' --hash=sha256:de526c12914f0c550d15924c62d72abc48d6fe7364aa87328337a31007fe8a4f --hash=sha256:939de3e7a6161af0c887ef91b7d41a53e7c5a1ca976325f429cb46ea9bc30ecc! Will try again.
;; An error occurred while installing towncrier==22.8.0; python_version >= '3.7' --hash=sha256:3b780c3d966e1b26414830aec3d15000654b31e64e024f3e5fd128b4c6eb8f47 --hash=sha256:7d3839b033859b45fb55df82b74cfd702431933c0cc9f287a5a7ea3e05d042cb! Will try again.
;; An error occurred while installing typing-extensions==4.3.0 --hash=sha256:e6d2677a32f47fc7eb2795db1dd15c1f34eff616bcaf2cfb5e997f854fa1c4a6 --hash=sha256:25642c956049920a5aa49edcdd6ab1e06d7e5d467fc00e0506c44ac86fbfca02! Will try again.
;; An error occurred while installing urllib3==1.26.12; python_version >= '2.7' and python_version not in '3.0, 3.1, 3.2, 3.3, 3.4, 3.5' and python_version < '4' --hash=sha256:3fa96cf423e6987997fc326ae8df396db2a8b7c667747d47ddd8ecba91f4a74e --hash=sha256:b930dd878d5a8afb066a637fbb35144fe7901e3b209d1cd4f524bd0e9deee997! Will try again.
;; An error occurred while installing virtualenv==20.16.4; python_version >= '3.6' --hash=sha256:035ed57acce4ac35c82c9d8802202b0e71adac011a511ff650cbcf9635006a22 --hash=sha256:014f766e4134d0008dcaa1f95bafa0fb0f575795d07cae50b1bee514185d6782! Will try again.
;; An error occurred while installing virtualenv-clone==0.5.7; python_version >= '2.7' and python_version not in '3.0, 3.1, 3.2, 3.3' --hash=sha256:44d5263bceed0bac3e1424d64f798095233b64def1c5689afa43dc3223caf5b0 --hash=sha256:418ee935c36152f8f153c79824bb93eaf6f0f7984bae31d3f48f350b9183501a! Will try again.
;; An error occurred while installing waitress==2.1.2; sys_platform == 'win32' --hash=sha256:780a4082c5fbc0fde6a2fcfe5e26e6efc1e8f425730863c04085769781f51eba --hash=sha256:7500c9625927c8ec60f54377d590f67b30c8e70ef4b8894214ac6e4cad233d2a! Will try again.
;; An error occurred while installing werkzeug==2.2.2; python_version >= '3.7' --hash=sha256:7ea2d48322cc7c0f8b3a215ed73eabd7b5d75d0b50e31ab006286ccff9e00b8f --hash=sha256:f979ab81f58d7318e064e99c4506445d60135ac5cd2e177a2de0089bfd4c9bd5! Will try again.
;; An error occurred while installing zipp==3.6.0; python_version < '3.10' --hash=sha256:9fe5ea21568a0a70e50f273397638d39b03353731e6cbbb3fd8502a33fec40bc --hash=sha256:71c644c5369f4a6e07636f0aa966270449561fcea2e3d6747b8d23efaa9d7832! Will try again.
;; An error occurred while installing -e .[dev,tests]! Will try again.
;; An error occurred while installing -e ./tests/pytest-pypi! Will try again.


;; (define-public python-pipenv
;;   (package
;;     (name "python-pipenv")
;;     (version "september-09-2022")

;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/pypa/pipenv")
;;              (commit "7d94c43128b8cab2488495ddcc7cfd19d70029dd")))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32 "02n407a6w0zy40by9njjkzpx0jbx24bddn2nxlg924d1yrmg90ff"))))
;;     (build-system python-build-system)
;;     (native-inputs
;;      (list python-wheel python-virtualenv))
;;     (arguments
;;      (list
;;       #:phases
;;       #~(modify-phases %standard-phases
;;           (replace 'check
;;             (lambda* (#:key tests? #:allow-other-keys)
;;               (setenv "HOME" "/tmp") ; test_with_pip tries to
;;               (if tests?
;;                   (begin
;;                     (invoke "./run-tests.sh"))
;;                   (format #t "test suite not run~%"))))
;;           )))
;;     (home-page "https://github.com/pypa/pipenv")
;;     (synopsis "")
;;     (description
;;      "")
;;     (license license:expat)))





;; propagatedBuildInputs = with python3.pkgs; [
;;     aws-lambda-builders ; n
;;     aws-sam-translator ; y
;;     chevron ; Y
;;     click ; y
;;     cookiecutter ; y
;;     dateparser ; y
;;     python-dateutil ; y
;;     docker ; y
;;     flask ; y
;;     jmespath ; y
;;     requests ; y
;;     serverlessrepo ; n
;;     tomlkit ; y
;;     watchdog ; y
;;     typing-extensions ; y
;;     regex ; y
;;   ];

(define-public aws-sam-cli2
  (package
    (name "aws-sam-cli")
    (version "1.37.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri name version))
        (sha256
         (base32
          "005dh2y45x92zl8sf2sqjmfvcqr4hrz8dfckgkckv87003v7lwqc"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'skip-failing-tests
           (lambda _
             ;; Skip tests that require network access.
             (substitute* "schema_salad/tests/test_cwl11.py"
               (("^def test_(secondaryFiles|outputBinding)" all)
                (string-append "@pytest.mark.skip(reason="
                               "\"test requires network access\")\n"
                               all))))))))
    (propagated-inputs
     (list python-cachecontrol
           python-lockfile
           python-mistune
           python-rdflib
           python-rdflib-jsonld
           python-requests
           python-ruamel.yaml
           python-typing-extensions))
    (native-inputs
     (list python-black python-pytest python-pytest-runner))
    (home-page "https://github.com/common-workflow-language/schema_salad")
    (synopsis "Schema Annotations for Linked Avro Data (SALAD)")
    (description
     "Salad is a schema language for describing JSON or YAML structured linked
data documents.  Salad schema describes rules for preprocessing, structural
validation, and hyperlink checking for documents described by a Salad schema.
Salad supports rich data modeling with inheritance, template specialization,
object identifiers, object references, documentation generation, code
generation, and transformation to RDF.  Salad provides a bridge between document
and record oriented data modeling and the Semantic Web.")
    (license license:asl2.0)))

aws-sam-cli2
python-chevron
;; python-serverlessrepo
;; python-pipenv
;; python-aws-lambda-builders
python-pyelftools