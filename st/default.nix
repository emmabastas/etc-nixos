{ st
, harfbuzz
, fetchpatch
, writeText
}:

st.overrideAttrs (oldAttrs: rec {
  # ligatures dependency
  buildInputs = oldAttrs.buildInputs ++ [ harfbuzz ];
  patches = [
    # ligatures patch
    (fetchpatch {
      url = "https://st.suckless.org/patches/ligatures/0.8.4/st-ligatures-20210824-0.8.4.diff";
      hash = "sha256-uRP9dijVDGjHaLe+kNo1boDGkM/dGonVZIdMwDKb72Y=";
    })
  ];
  configFile = writeText "config.def.h" (builtins.readFile ./st.h);
  postPatch = "${oldAttrs.postPatch}\n cp ${configFile} config.def.h";
})
