public class TestOpConstants {
  Object op_aconst_null () {
    return null;
  }

  byte op_bipush () {
    return 127;
  }

  short op_sipush () {
    return 32767;
  }

  int op_iconst_m1 () {
    return -1;
  }

  int op_iconst_0 () {
    return 0;
  }

  int op_iconst_1 () {
    return 1;
  }

  int op_iconst_2 () {
    return 2;
  }

  int op_iconst_3 () {
    return 3;
  }

  int op_iconst_4 () {
    return 4;
  }

  int op_iconst_5 () {
    return 5;
  }

  float op_fconst_0 () {
    return 0f;
  }

  float op_fconst_1 () {
    return 1f;
  }

  float op_fconst_2 () {
    return 2f;
  }

  long op_lconst_0 () {
    return 0L;
  }

  long op_lconst_1 () {
    return 1L;
  }

  double op_dconst_0 () {
    return 0d;
  }

  double op_dconst_1 () {
    return 1d;
  }

  int op_ldc_int () {
    return 2147483647;
  }

  float op_ldc_float () {
    return 3f;
  }

  String op_ldc_string () {
    return "你好";
  }

  Class op_ldc_class () { // TODO
    return Object.class;
  }

  private void make_256_constant_pool_entries () {
    int b=32767;b=32768;b=32769;b=32770;b=32771;b=32772;b=32773;b=32774;b=32775;b=32776;b=32777;b=32778;b=32779;b=32780;b=32781;b=32782;b=32783;b=32784;b=32785;b=32786;b=32787;b=32788;b=32789;b=32790;b=32791;b=32792;b=32793;b=32794;b=32795;b=32796;b=32797;b=32798;b=32799;b=32800;b=32801;b=32802;b=32803;b=32804;b=32805;b=32806;b=32807;b=32808;b=32809;b=32810;b=32811;b=32812;b=32813;b=32814;b=32815;b=32816;b=32817;b=32818;b=32819;b=32820;b=32821;b=32822;b=32823;b=32824;b=32825;b=32826;b=32827;b=32828;b=32829;b=32830;b=32831;b=32832;b=32833;b=32834;b=32835;b=32836;b=32837;b=32838;b=32839;b=32840;b=32841;b=32842;b=32843;b=32844;b=32845;b=32846;b=32847;b=32848;b=32849;b=32850;b=32851;b=32852;b=32853;b=32854;b=32855;b=32856;b=32857;b=32858;b=32859;b=32860;b=32861;b=32862;b=32863;b=32864;b=32865;b=32866;b=32867;b=32868;b=32869;b=32870;b=32871;b=32872;b=32873;b=32874;b=32875;b=32876;b=32877;b=32878;b=32879;b=32880;b=32881;b=32882;b=32883;b=32884;b=32885;b=32886;b=32887;b=32888;b=32889;b=32890;b=32891;b=32892;b=32893;b=32894;b=32895;b=32896;b=32897;b=32898;b=32899;b=32900;b=32901;b=32902;b=32903;b=32904;b=32905;b=32906;b=32907;b=32908;b=32909;b=32910;b=32911;b=32912;b=32913;b=32914;b=32915;b=32916;b=32917;b=32918;b=32919;b=32920;b=32921;b=32922;b=32923;b=32924;b=32925;b=32926;b=32927;b=32928;b=32929;b=32930;b=32931;b=32932;b=32933;b=32934;b=32935;b=32936;b=32937;b=32938;b=32939;b=32940;b=32941;b=32942;b=32943;b=32944;b=32945;b=32946;b=32947;b=32948;b=32949;b=32950;b=32951;b=32952;b=32953;b=32954;b=32955;b=32956;b=32957;b=32958;b=32959;b=32960;b=32961;b=32962;b=32963;b=32964;b=32965;b=32966;b=32967;b=32968;b=32969;b=32970;b=32971;b=32972;b=32973;b=32974;b=32975;b=32976;b=32977;b=32978;b=32979;b=32980;b=32981;b=32982;b=32983;b=32984;b=32985;b=32986;b=32987;b=32988;b=32989;b=32990;b=32991;b=32992;b=32993;b=32994;b=32995;b=32996;b=32997;b=32998;b=32999;b=33000;b=33001;b=33002;b=33003;b=33004;b=33005;b=33006;b=33007;b=33008;b=33009;b=33010;b=33011;b=33012;b=33013;b=33014;b=33015;b=33016;b=33017;b=33018;b=33019;b=33020;b=33021;b=33022;
  }

  int op_ldc_w_int () {
    return 2147483646;
  }

  float op_ldc_w_float () {
    return 9f;
  }

  String op_ldc_w_string () {
    return "世界";
  }

  Class op_ldc_w_class () { // TODO
    return Class.class;
  }

  long op_ldc2_w_long () {
    return 3L;
  }

  double op_ldc2_w_double () {
    return 3d;
  }
}
