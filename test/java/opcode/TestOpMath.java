public class TestOpMath {
  static int op_iadd (int a, int b) { return a + b; }

  static long op_ladd (long a, long b) { return a + b; }

  static float op_fadd (float a, float b) { return a + b; }

  static double op_dadd (double a, double b) { return a + b; }

  static int op_isub (int a, int b) { return a - b; }

  static long op_lsub (long a, long b) { return a - b; }

  static float op_fsub (float a, float b) { return a - b; }

  static double op_dsub (double a, double b) { return a - b; }

  static int op_imul (int a, int b) { return a * b; }

  static long op_lmul (long a, long b) { return a * b; }

  static float op_fmul (float a, float b) { return a * b; }

  static double op_dmul (double a, double b) { return a * b; }

  static int op_idiv (int a, int b) { return a / b; }

  static long op_ldiv (long a, long b) { return a / b; }

  static float op_fdiv (float a, float b) { return a / b; }

  static double op_ddiv (double a, double b) { return a / b; }

  static int op_irem (int a, int b) { return a % b; }

  static long op_lrem (long a, long b) { return a % b; }

  static float op_frem (float a, float b) { return a % b; }

  static double op_drem (double a, double b) { return a % b; }

  static int op_ineg (int a) { return -a; }

  static long op_lneg (long a) { return -a; }

  static float op_fneg (float a) { return -a; }

  static double op_dneg (double a) { return -a; }

  static int op_ishl (int a, int shift) { return a << shift; }

  static long op_lshl (long a, int shift) { return a << shift; }

  static int op_ishr (int a, int shift) { return a >> shift; }

  static long op_lshr (long a, int shift) { return a >> shift; }

  static int op_iushr (int a, int shift) { return a >>> shift; }

  static long op_lushr (long a, int shift) { return a >>> shift; }

  static int op_iand (int a, int b) { return a & b; }

  static long op_land (long a, long b) { return a & b; }

  static int op_ior (int a, int b) { return a | b; }

  static long op_lor (long a, long b) { return a | b; }

  static int op_ixor (int a, int b) { return a ^ b; }

  static long op_lxor (long a, long b) { return a ^ b; }

  static int op_iinc (int a) { return ++a ; }
}
