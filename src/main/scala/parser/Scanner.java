/* The following code was generated by JFlex 1.6.1 */

package parser;


/**
 * This class is a scanner generated by 
 * <a href="http://www.jflex.de/">JFlex</a> 1.6.1
 * from the specification file <tt>src/main/scala/parser/Scanner.flex</tt>
 */
class Scanner {

  /** This character denotes the end of file */
  public static final int YYEOF = -1;

  /** initial size of the lookahead buffer */
  private static final int ZZ_BUFFERSIZE = 16384;

  /** lexical states */
  public static final int YYINITIAL = 0;
  public static final int COMMENT = 2;

  /**
   * ZZ_LEXSTATE[l] is the state in the DFA for the lexical state l
   * ZZ_LEXSTATE[l+1] is the state in the DFA for the lexical state l
   *                  at the beginning of a line
   * l is of the form l = 2*k, k a non negative integer
   */
  private static final int ZZ_LEXSTATE[] = { 
     0,  0,  1, 1
  };

  /** 
   * Translates characters to character classes
   */
  private static final String ZZ_CMAP_PACKED = 
    "\11\0\1\3\1\2\1\47\1\50\1\1\22\0\1\3\1\6\1\45"+
    "\3\0\1\17\1\0\1\11\1\11\1\11\1\11\1\0\1\12\1\44"+
    "\1\11\12\4\1\0\1\51\1\6\1\10\1\7\2\0\32\5\1\0"+
    "\1\46\2\0\1\5\1\0\1\13\1\14\1\37\1\33\1\23\1\24"+
    "\1\41\1\40\1\30\1\5\1\43\1\25\1\31\1\26\1\27\1\32"+
    "\1\42\1\21\1\15\1\20\1\22\1\36\1\34\1\5\1\35\1\5"+
    "\1\0\1\16\10\0\1\47\u1fa2\0\1\47\1\47\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\udfe6\0";

  /** 
   * Translates characters to character classes
   */
  private static final char [] ZZ_CMAP = zzUnpackCMap(ZZ_CMAP_PACKED);

  /** 
   * Translates DFA states to action switch labels.
   */
  private static final int [] ZZ_ACTION = zzUnpackAction();

  private static final String ZZ_ACTION_PACKED_0 =
    "\2\0\1\1\2\2\1\3\1\4\1\5\1\1\2\5"+
    "\2\4\2\1\14\4\1\1\1\6\2\7\1\0\1\3"+
    "\13\4\1\5\2\4\1\5\11\4\1\0\1\10\1\0"+
    "\1\3\24\4\1\0\16\4\3\0\15\4\4\0\10\4"+
    "\1\5\1\0\1\4\4\0\5\4\4\0\4\4\3\0"+
    "\1\4\12\0";

  private static int [] zzUnpackAction() {
    int [] result = new int[158];
    int offset = 0;
    offset = zzUnpackAction(ZZ_ACTION_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackAction(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }


  /** 
   * Translates a state to a row index in the transition table
   */
  private static final int [] ZZ_ROWMAP = zzUnpackRowMap();

  private static final String ZZ_ROWMAP_PACKED_0 =
    "\0\0\0\52\0\124\0\176\0\124\0\250\0\322\0\374"+
    "\0\u0126\0\124\0\u0150\0\u017a\0\u01a4\0\u01ce\0\u01f8\0\u0222"+
    "\0\u024c\0\u0276\0\u02a0\0\u02ca\0\u02f4\0\u031e\0\u0348\0\u0372"+
    "\0\u039c\0\u03c6\0\u03f0\0\u041a\0\124\0\u0444\0\124\0\u046e"+
    "\0\u0498\0\u04c2\0\u04ec\0\u0516\0\u0540\0\u056a\0\u0594\0\u05be"+
    "\0\u05e8\0\u0612\0\u063c\0\u0666\0\322\0\u0690\0\u06ba\0\u06e4"+
    "\0\u070e\0\u0738\0\u0762\0\u078c\0\u07b6\0\u07e0\0\u080a\0\u0834"+
    "\0\u085e\0\u041a\0\124\0\u0888\0\u046e\0\u08b2\0\u08dc\0\u0906"+
    "\0\u0930\0\u095a\0\u0984\0\u09ae\0\u09d8\0\u0a02\0\u0a2c\0\u0a56"+
    "\0\u0a80\0\u0aaa\0\u0ad4\0\u0afe\0\u0b28\0\u0b52\0\u0b7c\0\u0ba6"+
    "\0\u0bd0\0\u0bfa\0\u0c24\0\u0c4e\0\u0c78\0\u0ca2\0\u0ccc\0\u0cf6"+
    "\0\u0d20\0\u0d4a\0\u0d74\0\u0d9e\0\u0dc8\0\u0df2\0\u0e1c\0\u0e46"+
    "\0\u0e70\0\u0e9a\0\u0ec4\0\u0eee\0\u0f18\0\u0f42\0\u0f6c\0\u0f96"+
    "\0\u0fc0\0\u0fea\0\u1014\0\u103e\0\u1068\0\u1092\0\u10bc\0\u10e6"+
    "\0\u1110\0\u113a\0\u1164\0\u118e\0\u11b8\0\u11e2\0\u120c\0\u1236"+
    "\0\u1260\0\u128a\0\u12b4\0\u12de\0\u11b8\0\u1308\0\u1332\0\u135c"+
    "\0\u1386\0\u13b0\0\u13da\0\u1404\0\u142e\0\u1458\0\u1482\0\u14ac"+
    "\0\u14d6\0\u1500\0\u152a\0\u1554\0\u157e\0\u15a8\0\u15d2\0\u15fc"+
    "\0\u1626\0\u1650\0\u167a\0\u16a4\0\u16ce\0\u16f8\0\u1722\0\u174c"+
    "\0\u1776\0\u17a0\0\u17ca\0\u17f4\0\u181e\0\u1848";

  private static int [] zzUnpackRowMap() {
    int [] result = new int[158];
    int offset = 0;
    offset = zzUnpackRowMap(ZZ_ROWMAP_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackRowMap(String packed, int offset, int [] result) {
    int i = 0;  /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int high = packed.charAt(i++) << 16;
      result[j++] = high | packed.charAt(i++);
    }
    return j;
  }

  /** 
   * The transition table of the DFA
   */
  private static final int [] ZZ_TRANS = zzUnpackTrans();

  private static final String ZZ_TRANS_PACKED_0 =
    "\1\3\1\4\2\5\1\6\1\7\2\10\1\11\1\12"+
    "\1\13\1\14\1\7\1\15\1\16\1\17\1\20\1\21"+
    "\1\7\1\22\1\23\1\24\1\25\1\26\1\27\1\7"+
    "\1\30\1\31\3\7\1\32\2\7\1\33\1\7\1\3"+
    "\1\34\2\3\1\5\1\35\1\5\1\36\1\37\44\5"+
    "\2\0\1\5\54\0\1\5\53\0\1\6\1\7\5\0"+
    "\3\7\2\0\24\7\1\40\11\0\2\7\5\0\3\7"+
    "\2\0\24\7\16\0\1\12\50\0\2\12\45\0\1\41"+
    "\51\0\2\7\5\0\1\7\1\42\1\7\2\0\5\7"+
    "\1\43\1\44\15\7\12\0\2\7\5\0\3\7\2\0"+
    "\3\7\1\45\4\7\1\46\13\7\24\0\1\12\52\0"+
    "\1\12\36\0\2\7\5\0\3\7\2\0\1\7\1\47"+
    "\22\7\12\0\2\7\5\0\3\7\2\0\7\7\1\50"+
    "\14\7\12\0\2\7\5\0\3\7\2\0\16\7\1\51"+
    "\5\7\12\0\2\7\5\0\1\52\2\7\2\0\24\7"+
    "\12\0\2\7\5\0\3\7\2\0\7\7\1\53\14\7"+
    "\12\0\2\7\5\0\3\7\2\0\7\7\1\54\14\7"+
    "\12\0\2\7\5\0\3\7\2\0\1\7\1\55\1\56"+
    "\7\7\1\57\11\7\12\0\2\7\5\0\3\7\2\0"+
    "\6\7\1\60\2\7\1\61\12\7\12\0\2\7\5\0"+
    "\1\62\2\7\2\0\2\7\1\63\4\7\1\64\1\65"+
    "\13\7\12\0\2\7\5\0\3\7\2\0\3\7\1\66"+
    "\20\7\12\0\2\7\5\0\3\7\2\0\5\7\1\67"+
    "\1\7\1\70\14\7\12\0\2\7\5\0\3\7\2\0"+
    "\2\7\1\71\21\7\6\0\45\72\1\73\1\74\3\72"+
    "\2\0\1\37\53\0\1\75\51\0\1\41\37\0\1\40"+
    "\11\0\2\7\5\0\2\7\1\55\2\0\24\7\12\0"+
    "\2\7\5\0\3\7\2\0\14\7\1\76\7\7\12\0"+
    "\2\7\5\0\3\7\2\0\13\7\1\55\10\7\12\0"+
    "\2\7\5\0\3\7\2\0\1\77\4\7\1\100\16\7"+
    "\12\0\2\7\5\0\3\7\2\0\11\7\1\101\12\7"+
    "\12\0\2\7\5\0\3\7\2\0\2\7\1\102\21\7"+
    "\12\0\2\7\5\0\1\7\1\103\1\7\2\0\24\7"+
    "\12\0\2\7\5\0\3\7\2\0\3\7\1\104\20\7"+
    "\12\0\2\7\5\0\3\7\2\0\5\7\1\105\16\7"+
    "\12\0\2\7\5\0\1\44\2\7\2\0\24\7\12\0"+
    "\2\7\5\0\3\7\2\0\1\55\23\7\12\0\2\7"+
    "\5\0\3\7\2\0\1\106\23\7\12\0\2\7\5\0"+
    "\3\7\2\0\1\107\23\7\12\0\2\7\5\0\3\7"+
    "\2\0\12\7\1\110\4\7\1\111\4\7\12\0\2\7"+
    "\5\0\3\7\2\0\12\7\1\112\11\7\12\0\2\7"+
    "\5\0\3\7\2\0\1\7\1\113\22\7\12\0\2\7"+
    "\5\0\2\7\1\114\2\0\24\7\12\0\2\7\5\0"+
    "\3\7\2\0\12\7\1\55\11\7\12\0\2\7\5\0"+
    "\3\7\2\0\3\7\1\115\20\7\12\0\2\7\5\0"+
    "\3\7\2\0\4\7\1\116\17\7\12\0\2\7\5\0"+
    "\3\7\2\0\3\7\1\117\20\7\12\0\2\7\5\0"+
    "\3\7\2\0\6\7\1\120\15\7\12\0\2\7\5\0"+
    "\3\7\2\0\10\7\1\54\13\7\6\0\1\72\2\0"+
    "\44\72\2\0\1\72\4\0\2\7\5\0\1\121\2\7"+
    "\2\0\24\7\12\0\2\7\4\0\1\122\3\7\2\0"+
    "\24\7\12\0\2\7\5\0\3\7\2\0\3\7\1\123"+
    "\20\7\12\0\2\7\5\0\3\7\2\0\2\7\1\124"+
    "\21\7\12\0\2\7\5\0\3\7\2\0\3\7\1\55"+
    "\20\7\12\0\2\7\5\0\3\7\2\0\2\7\1\125"+
    "\21\7\12\0\2\7\5\0\3\7\2\0\6\7\1\126"+
    "\15\7\12\0\2\7\5\0\2\7\1\127\2\0\24\7"+
    "\12\0\2\7\5\0\3\7\2\0\12\7\1\110\11\7"+
    "\12\0\2\7\5\0\3\7\2\0\10\7\1\130\13\7"+
    "\12\0\2\7\5\0\3\7\2\0\2\7\1\131\21\7"+
    "\12\0\2\7\5\0\3\7\2\0\5\7\1\132\16\7"+
    "\12\0\2\7\5\0\3\7\2\0\5\7\1\133\16\7"+
    "\12\0\2\7\5\0\1\134\2\7\2\0\24\7\12\0"+
    "\2\7\5\0\3\7\2\0\20\7\1\55\3\7\12\0"+
    "\2\7\5\0\3\7\2\0\17\7\1\135\4\7\12\0"+
    "\2\7\5\0\3\7\2\0\10\7\1\136\13\7\12\0"+
    "\2\7\5\0\1\137\2\7\2\0\24\7\12\0\2\7"+
    "\5\0\2\7\1\140\2\0\24\7\12\0\2\7\5\0"+
    "\3\7\2\0\15\7\1\42\6\7\23\0\1\141\3\0"+
    "\1\142\3\0\1\143\30\0\2\7\5\0\3\7\2\0"+
    "\17\7\1\144\4\7\12\0\2\7\5\0\3\7\2\0"+
    "\5\7\1\145\16\7\12\0\2\7\5\0\2\7\1\146"+
    "\2\0\24\7\12\0\2\7\5\0\3\7\2\0\1\147"+
    "\23\7\12\0\2\7\5\0\3\7\2\0\3\7\1\55"+
    "\4\7\1\150\13\7\12\0\2\7\5\0\3\7\2\0"+
    "\7\7\1\151\14\7\12\0\2\7\5\0\3\7\2\0"+
    "\1\42\23\7\12\0\2\7\5\0\3\7\2\0\2\7"+
    "\1\152\21\7\12\0\2\7\5\0\3\7\2\0\10\7"+
    "\1\153\13\7\12\0\2\7\5\0\3\7\2\0\11\7"+
    "\1\154\12\7\12\0\2\7\5\0\3\7\2\0\3\7"+
    "\1\155\20\7\12\0\2\7\5\0\3\7\2\0\6\7"+
    "\1\156\15\7\12\0\2\7\5\0\3\7\2\0\1\7"+
    "\1\157\22\7\12\0\2\7\5\0\3\7\2\0\1\160"+
    "\23\7\31\0\1\161\3\0\1\162\45\0\1\163\55\0"+
    "\1\164\26\0\2\7\5\0\3\7\2\0\1\165\23\7"+
    "\12\0\2\7\5\0\1\166\2\7\2\0\10\7\1\167"+
    "\13\7\12\0\2\7\5\0\3\7\2\0\1\170\23\7"+
    "\12\0\2\7\5\0\3\7\2\0\2\7\1\171\21\7"+
    "\12\0\2\7\5\0\3\7\2\0\4\7\1\172\17\7"+
    "\12\0\2\7\5\0\3\7\2\0\6\7\1\55\15\7"+
    "\12\0\2\7\5\0\3\7\2\0\13\7\1\102\10\7"+
    "\12\0\2\7\5\0\3\7\2\0\3\7\1\42\20\7"+
    "\12\0\2\7\5\0\3\7\2\0\3\7\1\173\20\7"+
    "\12\0\2\7\5\0\3\7\2\0\14\7\1\174\7\7"+
    "\12\0\2\7\5\0\3\7\2\0\3\7\1\175\20\7"+
    "\12\0\2\7\4\0\1\176\3\7\2\0\24\7\12\0"+
    "\2\7\5\0\1\177\2\7\2\0\24\7\31\0\1\200"+
    "\53\0\1\201\56\0\1\202\60\0\1\12\14\0\2\7"+
    "\4\0\1\203\3\7\2\0\24\7\12\0\2\7\5\0"+
    "\3\7\2\0\1\102\23\7\12\0\2\7\5\0\3\7"+
    "\2\0\6\7\1\204\15\7\12\0\2\7\5\0\3\7"+
    "\2\0\6\7\1\205\15\7\12\0\2\7\5\0\1\206"+
    "\2\7\2\0\24\7\12\0\2\7\5\0\3\7\2\0"+
    "\15\7\1\55\6\7\12\0\2\7\5\0\3\7\2\0"+
    "\1\207\23\7\12\0\2\7\5\0\3\7\2\0\10\7"+
    "\1\210\13\7\23\0\1\211\7\0\1\143\30\0\2\7"+
    "\5\0\3\7\2\0\6\7\1\54\15\7\41\0\1\12"+
    "\54\0\1\212\36\0\1\213\43\0\1\214\40\0\2\7"+
    "\5\0\3\7\2\0\23\7\1\55\12\0\2\7\5\0"+
    "\3\7\2\0\3\7\1\215\20\7\12\0\2\7\5\0"+
    "\3\7\2\0\5\7\1\216\16\7\12\0\2\7\5\0"+
    "\3\7\2\0\3\7\1\217\20\7\12\0\2\7\5\0"+
    "\2\7\1\220\2\0\24\7\31\0\1\161\51\0\1\221"+
    "\41\0\1\222\73\0\1\223\20\0\2\7\5\0\2\7"+
    "\1\42\2\0\24\7\12\0\2\7\5\0\3\7\2\0"+
    "\5\7\1\172\16\7\12\0\2\7\5\0\3\7\2\0"+
    "\1\7\1\42\22\7\12\0\2\7\5\0\3\7\2\0"+
    "\3\7\1\224\20\7\27\0\1\12\50\0\1\12\46\0"+
    "\1\225\40\0\2\7\4\0\1\226\3\7\2\0\24\7"+
    "\26\0\1\227\70\0\1\230\35\0\1\231\55\0\1\232"+
    "\53\0\1\12\46\0\1\233\40\0\1\234\54\0\1\235"+
    "\44\0\1\236\64\0\1\222\23\0";

  private static int [] zzUnpackTrans() {
    int [] result = new int[6258];
    int offset = 0;
    offset = zzUnpackTrans(ZZ_TRANS_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackTrans(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      value--;
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }


  /* error codes */
  private static final int ZZ_UNKNOWN_ERROR = 0;
  private static final int ZZ_NO_MATCH = 1;
  private static final int ZZ_PUSHBACK_2BIG = 2;

  /* error messages for the codes above */
  private static final String ZZ_ERROR_MSG[] = {
    "Unknown internal scanner error",
    "Error: could not match input",
    "Error: pushback value was too large"
  };

  /**
   * ZZ_ATTRIBUTE[aState] contains the attributes of state <code>aState</code>
   */
  private static final int [] ZZ_ATTRIBUTE = zzUnpackAttribute();

  private static final String ZZ_ATTRIBUTE_PACKED_0 =
    "\2\0\1\11\1\1\1\11\4\1\1\11\22\1\1\11"+
    "\1\1\1\11\1\0\31\1\1\0\1\11\1\0\25\1"+
    "\1\0\16\1\3\0\15\1\4\0\11\1\1\0\1\1"+
    "\4\0\5\1\4\0\4\1\3\0\1\1\12\0";

  private static int [] zzUnpackAttribute() {
    int [] result = new int[158];
    int offset = 0;
    offset = zzUnpackAttribute(ZZ_ATTRIBUTE_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackAttribute(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }

  /** the input device */
  private java.io.Reader zzReader;

  /** the current state of the DFA */
  private int zzState;

  /** the current lexical state */
  private int zzLexicalState = YYINITIAL;

  /** this buffer contains the current text to be matched and is
      the source of the yytext() string */
  private char zzBuffer[] = new char[ZZ_BUFFERSIZE];

  /** the textposition at the last accepting state */
  private int zzMarkedPos;

  /** the current text position in the buffer */
  private int zzCurrentPos;

  /** startRead marks the beginning of the yytext() string in the buffer */
  private int zzStartRead;

  /** endRead marks the last character in the buffer, that has been read
      from input */
  private int zzEndRead;

  /** number of newlines encountered up to the start of the matched text */
  private int yyline;

  /** the number of characters up to the start of the matched text */
  private int yychar;

  /**
   * the number of characters from the last newline up to the start of the 
   * matched text
   */
  private int yycolumn;

  /** 
   * zzAtBOL == true <=> the scanner is currently at the beginning of a line
   */
  private boolean zzAtBOL = true;

  /** zzAtEOF == true <=> the scanner is at the EOF */
  private boolean zzAtEOF;

  /** denotes if the user-EOF-code has already been executed */
  private boolean zzEOFDone;
  
  /** 
   * The number of occupied positions in zzBuffer beyond zzEndRead.
   * When a lead/high surrogate has been read from the input stream
   * into the final zzBuffer position, this will have a value of 1;
   * otherwise, it will have a value of 0.
   */
  private int zzFinalHighSurrogate = 0;

  /* user code: */
    public int line()   { return yyline; }
    public int column() { return yycolumn; }
    public int pos()    { return yychar; }
    
    public String text() {
        return yytext();
    }
    
    public String unquote() {
        String lit = text();
        return lit.substring(1, lit.length() - 1);
    }


  /**
   * Creates a new scanner
   *
   * @param   in  the java.io.Reader to read input from.
   */
  Scanner(java.io.Reader in) {
    this.zzReader = in;
  }


  /** 
   * Unpacks the compressed character translation table.
   *
   * @param packed   the packed character translation table
   * @return         the unpacked character translation table
   */
  private static char [] zzUnpackCMap(String packed) {
    char [] map = new char[0x110000];
    int i = 0;  /* index in packed string  */
    int j = 0;  /* index in unpacked array */
    while (i < 168) {
      int  count = packed.charAt(i++);
      char value = packed.charAt(i++);
      do map[j++] = value; while (--count > 0);
    }
    return map;
  }


  /**
   * Refills the input buffer.
   *
   * @return      <code>false</code>, iff there was new input.
   * 
   * @exception   java.io.IOException  if any I/O-Error occurs
   */
  private boolean zzRefill() throws java.io.IOException {

    /* first: make room (if you can) */
    if (zzStartRead > 0) {
      zzEndRead += zzFinalHighSurrogate;
      zzFinalHighSurrogate = 0;
      System.arraycopy(zzBuffer, zzStartRead,
                       zzBuffer, 0,
                       zzEndRead-zzStartRead);

      /* translate stored positions */
      zzEndRead-= zzStartRead;
      zzCurrentPos-= zzStartRead;
      zzMarkedPos-= zzStartRead;
      zzStartRead = 0;
    }

    /* is the buffer big enough? */
    if (zzCurrentPos >= zzBuffer.length - zzFinalHighSurrogate) {
      /* if not: blow it up */
      char newBuffer[] = new char[zzBuffer.length*2];
      System.arraycopy(zzBuffer, 0, newBuffer, 0, zzBuffer.length);
      zzBuffer = newBuffer;
      zzEndRead += zzFinalHighSurrogate;
      zzFinalHighSurrogate = 0;
    }

    /* fill the buffer with new input */
    int requested = zzBuffer.length - zzEndRead;
    int numRead = zzReader.read(zzBuffer, zzEndRead, requested);

    /* not supposed to occur according to specification of java.io.Reader */
    if (numRead == 0) {
      throw new java.io.IOException("Reader returned 0 characters. See JFlex examples for workaround.");
    }
    if (numRead > 0) {
      zzEndRead += numRead;
      /* If numRead == requested, we might have requested to few chars to
         encode a full Unicode character. We assume that a Reader would
         otherwise never return half characters. */
      if (numRead == requested) {
        if (Character.isHighSurrogate(zzBuffer[zzEndRead - 1])) {
          --zzEndRead;
          zzFinalHighSurrogate = 1;
        }
      }
      /* potentially more input available */
      return false;
    }

    /* numRead < 0 ==> end of stream */
    return true;
  }

    
  /**
   * Closes the input stream.
   */
  public final void yyclose() throws java.io.IOException {
    zzAtEOF = true;            /* indicate end of file */
    zzEndRead = zzStartRead;  /* invalidate buffer    */

    if (zzReader != null)
      zzReader.close();
  }


  /**
   * Resets the scanner to read from a new input stream.
   * Does not close the old reader.
   *
   * All internal variables are reset, the old input stream 
   * <b>cannot</b> be reused (internal buffer is discarded and lost).
   * Lexical state is set to <tt>ZZ_INITIAL</tt>.
   *
   * Internal scan buffer is resized down to its initial length, if it has grown.
   *
   * @param reader   the new input stream 
   */
  public final void yyreset(java.io.Reader reader) {
    zzReader = reader;
    zzAtBOL  = true;
    zzAtEOF  = false;
    zzEOFDone = false;
    zzEndRead = zzStartRead = 0;
    zzCurrentPos = zzMarkedPos = 0;
    zzFinalHighSurrogate = 0;
    yyline = yychar = yycolumn = 0;
    zzLexicalState = YYINITIAL;
    if (zzBuffer.length > ZZ_BUFFERSIZE)
      zzBuffer = new char[ZZ_BUFFERSIZE];
  }


  /**
   * Returns the current lexical state.
   */
  public final int yystate() {
    return zzLexicalState;
  }


  /**
   * Enters a new lexical state
   *
   * @param newState the new lexical state
   */
  public final void yybegin(int newState) {
    zzLexicalState = newState;
  }


  /**
   * Returns the text matched by the current regular expression.
   */
  public final String yytext() {
    return new String( zzBuffer, zzStartRead, zzMarkedPos-zzStartRead );
  }


  /**
   * Returns the character at position <tt>pos</tt> from the 
   * matched text. 
   * 
   * It is equivalent to yytext().charAt(pos), but faster
   *
   * @param pos the position of the character to fetch. 
   *            A value from 0 to yylength()-1.
   *
   * @return the character at position pos
   */
  public final char yycharat(int pos) {
    return zzBuffer[zzStartRead+pos];
  }


  /**
   * Returns the length of the matched text region.
   */
  public final int yylength() {
    return zzMarkedPos-zzStartRead;
  }


  /**
   * Reports an error that occured while scanning.
   *
   * In a wellformed scanner (no or only correct usage of 
   * yypushback(int) and a match-all fallback rule) this method 
   * will only be called with things that "Can't Possibly Happen".
   * If this method is called, something is seriously wrong
   * (e.g. a JFlex bug producing a faulty scanner etc.).
   *
   * Usual syntax/scanner level error handling should be done
   * in error fallback rules.
   *
   * @param   errorCode  the code of the errormessage to display
   */
  private void zzScanError(int errorCode) {
    String message;
    try {
      message = ZZ_ERROR_MSG[errorCode];
    }
    catch (ArrayIndexOutOfBoundsException e) {
      message = ZZ_ERROR_MSG[ZZ_UNKNOWN_ERROR];
    }

    throw new Error(message);
  } 


  /**
   * Pushes the specified amount of characters back into the input stream.
   *
   * They will be read again by then next call of the scanning method
   *
   * @param number  the number of characters to be read again.
   *                This number must not be greater than yylength()!
   */
  public void yypushback(int number)  {
    if ( number > yylength() )
      zzScanError(ZZ_PUSHBACK_2BIG);

    zzMarkedPos -= number;
  }


  /**
   * Resumes scanning until the next regular expression is matched,
   * the end of input is encountered or an I/O-Error occurs.
   *
   * @return      the next token
   * @exception   java.io.IOException  if any I/O-Error occurs
   */
  public Token next() throws java.io.IOException {
    int zzInput;
    int zzAction;

    // cached fields:
    int zzCurrentPosL;
    int zzMarkedPosL;
    int zzEndReadL = zzEndRead;
    char [] zzBufferL = zzBuffer;
    char [] zzCMapL = ZZ_CMAP;

    int [] zzTransL = ZZ_TRANS;
    int [] zzRowMapL = ZZ_ROWMAP;
    int [] zzAttrL = ZZ_ATTRIBUTE;

    while (true) {
      zzMarkedPosL = zzMarkedPos;

      boolean zzR = false;
      int zzCh;
      int zzCharCount;
      for (zzCurrentPosL = zzStartRead  ;
           zzCurrentPosL < zzMarkedPosL ;
           zzCurrentPosL += zzCharCount ) {
        zzCh = Character.codePointAt(zzBufferL, zzCurrentPosL, zzMarkedPosL);
        zzCharCount = Character.charCount(zzCh);
        switch (zzCh) {
        case '\u000B':
        case '\u000C':
        case '\u0085':
        case '\u2028':
        case '\u2029':
          yyline++;
          yycolumn = 0;
          zzR = false;
          break;
        case '\r':
          yyline++;
          yycolumn = 0;
          zzR = true;
          break;
        case '\n':
          if (zzR)
            zzR = false;
          else {
            yyline++;
            yycolumn = 0;
          }
          break;
        default:
          zzR = false;
          yycolumn += zzCharCount;
        }
      }

      if (zzR) {
        // peek one character ahead if it is \n (if we have counted one line too much)
        boolean zzPeek;
        if (zzMarkedPosL < zzEndReadL)
          zzPeek = zzBufferL[zzMarkedPosL] == '\n';
        else if (zzAtEOF)
          zzPeek = false;
        else {
          boolean eof = zzRefill();
          zzEndReadL = zzEndRead;
          zzMarkedPosL = zzMarkedPos;
          zzBufferL = zzBuffer;
          if (eof) 
            zzPeek = false;
          else 
            zzPeek = zzBufferL[zzMarkedPosL] == '\n';
        }
        if (zzPeek) yyline--;
      }
      zzAction = -1;

      zzCurrentPosL = zzCurrentPos = zzStartRead = zzMarkedPosL;
  
      zzState = ZZ_LEXSTATE[zzLexicalState];

      // set up zzAction for empty match case:
      int zzAttributes = zzAttrL[zzState];
      if ( (zzAttributes & 1) == 1 ) {
        zzAction = zzState;
      }


      zzForAction: {
        while (true) {
    
          if (zzCurrentPosL < zzEndReadL) {
            zzInput = Character.codePointAt(zzBufferL, zzCurrentPosL, zzEndReadL);
            zzCurrentPosL += Character.charCount(zzInput);
          }
          else if (zzAtEOF) {
            zzInput = YYEOF;
            break zzForAction;
          }
          else {
            // store back cached positions
            zzCurrentPos  = zzCurrentPosL;
            zzMarkedPos   = zzMarkedPosL;
            boolean eof = zzRefill();
            // get translated positions and possibly new buffer
            zzCurrentPosL  = zzCurrentPos;
            zzMarkedPosL   = zzMarkedPos;
            zzBufferL      = zzBuffer;
            zzEndReadL     = zzEndRead;
            if (eof) {
              zzInput = YYEOF;
              break zzForAction;
            }
            else {
              zzInput = Character.codePointAt(zzBufferL, zzCurrentPosL, zzEndReadL);
              zzCurrentPosL += Character.charCount(zzInput);
            }
          }
          int zzNext = zzTransL[ zzRowMapL[zzState] + zzCMapL[zzInput] ];
          if (zzNext == -1) break zzForAction;
          zzState = zzNext;

          zzAttributes = zzAttrL[zzState];
          if ( (zzAttributes & 1) == 1 ) {
            zzAction = zzState;
            zzMarkedPosL = zzCurrentPosL;
            if ( (zzAttributes & 8) == 8 ) break zzForAction;
          }

        }
      }

      // store back cached position
      zzMarkedPos = zzMarkedPosL;

      if (zzInput == YYEOF && zzStartRead == zzCurrentPos) {
        zzAtEOF = true;
        return null;
      }
      else {
        switch (zzAction < 0 ? zzAction : ZZ_ACTION[zzAction]) {
          case 1: 
            { throw new RuntimeException("Unexpected input '" + text() + "' at position " + pos());
            }
          case 9: break;
          case 2: 
            { /* ignore */
            }
          case 10: break;
          case 3: 
            { return new Literal(Double.parseDouble(text()));
            }
          case 11: break;
          case 4: 
            { return new Identifier(text());
            }
          case 12: break;
          case 5: 
            { return new Keyword(text());
            }
          case 13: break;
          case 6: 
            { yybegin(COMMENT);
            }
          case 14: break;
          case 7: 
            { yybegin(YYINITIAL);
            }
          case 15: break;
          case 8: 
            { return new Literal(unquote());
            }
          case 16: break;
          default:
            zzScanError(ZZ_NO_MATCH);
        }
      }
    }
  }


}
