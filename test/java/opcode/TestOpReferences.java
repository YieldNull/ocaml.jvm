public class TestOpReferences {
  static int i;

  // static final String str = "Hello";

  static long foo (long z, int p, long q) {
    int i = 10 * p;
    long r = i * 1024 + q;
    return r * z;
  }

  static {
    i = 100;
    i = i * 10;
  }

  int op_getstatic () {
    return i;
  }

  int op_putstatic () {
    i = 2;
    return i;
  }

  static long op_invokestatic () {
    int p = 5;
    long i = foo (6666, 1024, 5678);
    long j = i * 10 + p;
    return j;
  }

  public static void main(String[] args) {
    System.out.println(op_invokestatic());
  }
}
