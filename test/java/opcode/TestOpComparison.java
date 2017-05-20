public class TestOpComparison {
  static int op_if_icmpeq (int a, int b) {
    if (a != b)
      return 0;
    else
      return 1024;
  }

  static int op_if_icmpne (int a, int b) {
    if (a == b)
      return 0;
    else
      return 1024;
  }

  static int op_if_icmplt (int a, int b) {
    if (a >= b)
      return 0;
    else
      return 1024;
  }

  static int op_if_icmpge (int a, int b) {
    if (a < b)
      return 0;
    else
      return 1024;
  }

  static int op_if_icmpgt (int a, int b) {
    if (a <= b)
      return 0;
    else
      return 1024;
  }

  static int op_if_icmple (int a, int b) {
    if (a > b)
      return 0;
    else
      return 1024;
  }

  static int op_ifeq (int a) {
    if (a != 0)
      return 0;
    else
      return 1024;
  }

  static int op_ifne (int a) {
    if (a == 0)
      return 0;
    else
      return 1024;
  }

  static int op_ifgt (int a) {
    if (a <= 0)
      return 0;
    else
      return 1024;
  }

  static int op_ifge (int a) {
    if (a < 0)
      return 0;
    else
      return 1024;
  }

  static int op_iflt (int a) {
    if (a >= 0)
      return 0;
    else
      return 1024;
  }

  static int op_ifle (int a) {
    if (a > 0)
      return 0;
    else
      return 1024;
  }

  static int op_lcmp (long a, long b) {
    if (a < b)
      return 0;
    else
      return 1024;
  }

  static int op_fcmpg (float a, float b) {
    if (a < b)
      return 0;
    else
      return 1024;
  }

  static int op_fcmpl (float a, float b) {
    if (a > b)
      return 0;
    else
      return 1024;
  }

  static boolean op_if_acmpeq (String a, String b) {
    return a != b;
  }

  static boolean op_if_acmpne (String a, String b) {
    return a == b;
  }
}
