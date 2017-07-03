{ mkDerivation, array, base, colour, containers, diffarray, FTGL
, GLURaw, GLUT, HUnit, mtl, OpenGL, OpenGLRaw, parsec, process
, QuickCheck, random, stdenv, stm, test-framework
, test-framework-hunit, test-framework-quickcheck2, time, unix
, terminfo
, bash , readline
}:
mkDerivation {
  pname = "Terminal";
  version = "0.0.3";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    array base colour containers diffarray FTGL GLURaw GLUT mtl OpenGL
    OpenGLRaw parsec process random stm time unix terminfo
  ];
  testHaskellDepends = [
    base diffarray HUnit QuickCheck test-framework test-framework-hunit
    test-framework-quickcheck2
  ];
  executableSystemDepends = [ bash readline ];
  license = "GPL";
}
