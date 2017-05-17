public class TestOpStoreAndLoad {
  static int op_istore_0_iload_0 () {
    int a = -1;
    return a;
  }

  int op_istore_1_iload_1 () {
    int a = -1;
    return a;
  }

  int op_istore_2_iload_2 () {
    int a = 0;
    int b = -1;
    return b;
  }

  int op_istore_3_iload_3 () {
    int a = 1;
    int b = 0;
    int c = -1;
    return c;
  }

  int op_istore_iload () {
    int a = 2;
    int b = 1;
    int c = 0;
    int d = -1;
    return d;
  }

  static float op_fstore_0_fload_0 () {
    float a = -1f;
    return a;
  }

  float op_fstore_1_fload_1 () {
    float a = -1f;
    return a;
  }

  float op_fstore_2_fload_2 () {
    float a = 0f;
    float b = -1f;
    return b;
  }

  float op_fstore_3_fload_3 () {
    float a = 1f;
    float b = 0f;
    float c = -1f;
    return c;
  }

  float op_fstore_fload () {
    float a = 2f;
    float b = 1f;
    float c = 0f;
    float d = -1f;
    return d;
  }

  static long op_lstore_0_lload_0 () {
    long a = -1l;
    return a;
  }

  long op_lstore_1_lload_1 () {
    long a = -1l;
    return a;
  }

  long op_lstore_2_lload_2 () {
    int a = 0;
    long b = -1l;
    return b;
  }

  long op_lstore_3_lload_3 () {
    long a = 0l;
    long b = -1l;
    return b;
  }

  long op_lstore_lload () {
    long a = 1l;
    long b = 0l;
    long c = -1l;
    return c;
  }

  static double op_dstore_0_dload_0 () {
    double a = -1.0;
    return a;
  }

  double op_dstore_1_dload_1 () {
    double a = -1.0;
    return a;
  }

  double op_dstore_2_dload_2 () {
    int a = 0;
    double b = -1.0;
    return b;
  }

  double op_dstore_3_dload_3 () {
    double a = 0.0;
    double b = -1.0;
    return b;
  }

  double op_dstore_dload () {
    double a = 1.0;
    double b = 0.0;
    double c = -1.0;
    return c;
  }

  static Object op_astore_0_aload_0 () {
    Object a = null;
    return a;
  }

  Object op_astore_1_aload_1 () {
    Object a = null;
    return a;
  }

  Object op_astore_2_aload_2 () {
    int a = 1;
    Object b = null;
    return b;
  }

  Object op_astore_3_aload_3 () {
    int a = 0;
    int b = 1;
    Object c = null;
    return c;
  }

  Object op_astore_aload () {
    int a = 0;
    int b = 1;
    int c = 2;
    Object d = null;
    return d;
  }

  byte op_bastore_baload () { // TODO test exception
    byte[] arr = new byte[1];
    arr[0] = -1;
    return arr[0];
  }

  short op_sastore_saload () {
    short[] arr = new short[1];
    arr[0] = -1;
    return arr[0];
  }

  char op_castore_caload () {
    char[] arr = new char[1];
    arr[0] = '\uFFFF';
    return arr[0];
  }

  int op_iastore_iaload () {
    int[] arr = new int[1];
    arr[0] = -1;
    return arr[0];
  }

  float op_fastore_faload () {
    float[] arr = new float[1];
    arr[0] = -1.0f;
    return arr[0];
  }

  long op_lastore_laload () {
    long [] arr = new long[1];
    arr[0] = -1L;
    return arr[0];
  }

  double op_dastore_daload () {
    double[] arr = new double[1];
    arr[0] = -1.0;
    return arr[0];
  }

  Object op_aastore_aaload () {
    Object[] arr = new Object[1];
    arr[0] = null;
    return arr[0];
  }
}
