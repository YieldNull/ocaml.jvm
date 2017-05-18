public class TestOpConversions {

  long op_i2l () {
    int a = 0x7fffffff;
    return a;
  }

  float op_i2f () {
    int a = 0x7fffffff;
    return a;
  }

  double op_i2d () {
    int a = 0x7fffffff;
    return a;
  }

  int op_l2i () {
    long a = 0x7fffffffffffffffL;
    return (int)a;
  }

  float op_l2f () {
    long a = 0x7fffffffffffffffL;
    return (float)a;
  }

  double op_l2d () {
    long a = 0x7fffffffffffffffL;
    return a;
  }

  int op_f2i () {
    float a = 314.15926f;
    return (int) a;
  }

  long op_f2l () {
    float a = 314.15926f;
    return (long) a;
  }

  double op_f2d () {
    float a = 314.15926f;
    return (double) a;
  }

  int op_f2i_nan () {
    float a = 0.0f / 0.0f;
    return (int) a;
  }

  long op_f2l_nan () {
    float a = 0.0f / 0.0f;
    return (long) a;
  }

  double op_f2d_nan () {
    float a = 0.0f / 0.0f;
    return (double) a;
  }

  int op_f2i_inf_pos () {
    float a = 1.0f / 0.0f;
    return (int) a;
  }

  long op_f2l_inf_pos () {
    float a = 1.0f / 0.0f;
    return (long) a;
  }

  double op_f2d_inf_pos () {
    float a = 1.0f / 0.0f;
    return (double) a;
  }

  int op_f2i_inf_neg () {
    float a = -1.0f / 0.0f;
    return (int) a;
  }

  long op_f2l_inf_neg () {
    float a = -1.0f / 0.0f;
    return (long) a;
  }

  double op_f2d_inf_neg () {
    float a = -1.0f / 0.0f;
    return (double) a;
  }

  int op_d2i () {
    double a = 314.15926;
    return (int) a;
  }

  long op_d2l () {
    double a = 314.15926;
    return (long) a;
  }

  float op_d2f () {
    double a = 314.15926;
    return (float) a;
  }

  int op_d2i_nan () {
    double a = 0.0d / 0.0;
    return (int) a;
  }

  long op_d2l_nan () {
    double a = 0.0d / 0.0;
    return (long) a;
  }

  float op_d2f_nan () {
    double a = 0.0d / 0.0;
    return (float) a;
  }

  int op_d2i_inf_pos () {
    double a = 1.0 / 0.0;
    return (int) a;
  }

  long op_d2l_inf_pos () {
    double a = 1.0 / 0.0;
    return (long) a;
  }

  float op_d2f_inf_pos () {
    double a = 1.0 / 0.0;
    return (float) a;
  }

  int op_d2i_inf_neg () {
    double a = -1.0 / 0.0;
    return (int) a;
  }

  long op_d2l_inf_neg () {
    double a = -1.0 / 0.0;
    return (long) a;
  }

  float op_d2f_inf_neg () {
    double a = -1.0 / 0.0;
    return (float) a;
  }

  byte op_i2b_pos () {
    int a = 0x7fffff7f;

    return (byte) a;
  }

  char op_i2c_pos () {
    int a = 0x7fff7fff;

    return (char) a;
  }

  short op_i2s_pos () {
    int a = 0x7fff7fff;

    return (short) a;
  }

  byte op_i2b_neg () {
    int a = 0x7fffffff;

    return (byte) a;
  }

  char op_i2c_neg () {
    int a = 0x7fffffff;

    return (char) a;
  }

  short op_i2s_neg () {
    int a = 0x7fffffff;

    return (short) a;
  }
}
