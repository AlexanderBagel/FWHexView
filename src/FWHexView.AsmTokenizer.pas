////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : Hex Viewer Project
//  * Unit Name : FWHexView.AsmTokenizer.pas
//  * Purpose   : Class for defining the type of token with which the string begins
//  * Author    : Alexander (Rouse_) Bagel
//  * Copyright : © Fangorn Wizards Lab 1998 - 2025.
//  * Version   : 2.0.14
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//  * Latest Release : https://github.com/AlexanderBagel/FWHexView/releases
//  * Latest Source  : https://github.com/AlexanderBagel/FWHexView
//  ****************************************************************************
//

{
Licence:
  FWHexView is dual-licensed. You may choose to use it under the restrictions of the GPL v3 licence at no cost to you,
  or you may purchase a commercial licence. A commercial licence grants you the right to use FWHexView in your own
  applications, royalty free, and without any requirement to disclose your source code nor any modifications to FWHexView
  to any other party. A commercial licence lasts into perpetuity, and entitles you to all future updates, free of
  charge. A commercial licence is sold per developer developing applications that use FWHexView, as follows:
    1 developer = $49
    2 developers = $89
    3 developers = $139
    4 developers = $169
    5 developers = $199
    >5 developers = $199 + $25 per developer from the 6th onwards
    site licence = $499 (unlimited number of developers affiliated with the owner of the licence, i.e. employees, co-workers, interns and contractors)

  Please send an e-mail to hexview_sale@rousehome.ru to request an invoice before or after payment is made. Payment may be
  made via bank transfer. Bank details will be provided on the invoice.

  Support (via e-mail) is available for users with a commercial licence. Enhancement requests submitted by users with a
  commercial licence will be prioritized.
}

unit FWHexView.AsmTokenizer;

{$IFDEF FPC}
  {$I FWHexViewConfig.inc}
{$ENDIF}

interface

uses
  SysUtils,
  Generics.Collections;

type
  TTokenizerMode = (tmIntel, tmAtAntT, tmArm);
  TTokenType = (ttUnknown, ttNumber, ttInstruction, ttReg, ttPrefix, ttJmp, ttKernel, ttNop, ttSize);
  TRegType = (rtUnknown, rtReg8, rtReg16, rtReg32, rtReg64, rtRegSeg, rtRegPtr, rtX87, rtSingle, rtDouble, rtSimd64, rtSimd128, rtSimd256, rtSimd512);

  { TAsmTokenizer }

  TAsmTokenizer = class
  private
    FTokenizerMode: TTokenizerMode;
    FTokens: TDictionary<string, TTokenType>;
    function IsNumber(const Value: string): Boolean;
    procedure SetTokenizerMode(AValue: TTokenizerMode);
    procedure UpdateMode;
  public
    constructor Create;
    destructor Destroy; override;
    function GetToken(pData: PChar; var TokenLength: Integer): TTokenType;
    function GetRegType(const AReg: string): TRegType;
    property TokenizerMode: TTokenizerMode read FTokenizerMode write SetTokenizerMode;
  end;

implementation

const InstBuff: array[0..1359] of string = (
  'AAA', 'AAD', 'AAM', 'AAS', 'ADC', 'ADCX', 'ADD', 'ADDPD', 'ADDPS', 'ADDSD',
  'ADDSS', 'ADDSUBPD', 'ADDSUBPS', 'ADOX', 'AESDEC', 'AESDEC128KL',
  'AESDEC256KL', 'AESDECLAST', 'AESDECWIDE128KL', 'AESDECWIDE256KL', 'AESENC',
  'AESENC128KL', 'AESENC256KL', 'AESENCLAST', 'AESENCWIDE128KL',
  'AESENCWIDE256KL', 'AESIMC', 'AESKEYGENASSIST', 'AND', 'ANDN', 'ANDNPD',
  'ANDNPS', 'ANDPD', 'ANDPS', 'BEXTR', 'BLENDPD', 'BLENDPS', 'BLENDVPD',
  'BLENDVPS', 'BLSI', 'BLSMSK', 'BLSR', 'BNDCL', 'BNDCN', 'BNDCU', 'BNDLDX',
  'BNDMK', 'BNDMOV', 'BNDSTX', 'BOUND', 'BSF', 'BSR', 'BSWAP', 'BT', 'BTC',
  'BTR', 'BTS', 'BZHI', 'CALLF', 'CBW', 'CCS_ENCRYPT', 'CCS_HASH', 'CDQ',
  'CDQE', 'CLC', 'CLD', 'CLDEMOTE', 'CLFLUSH', 'CLFLUSHOPT', 'CLI', 'CLUI',
  'CLWB', 'CLZERO', 'CMC', 'CMOVA', 'CMOVAE', 'CMOVB', 'CMOVBE', 'CMOVC',
  'CMOVCC', 'CMOVE', 'CMOVG', 'CMOVGE', 'CMOVL', 'CMOVLE', 'CMOVNA', 'CMOVNAE',
  'CMOVNB', 'CMOVNBE', 'CMOVNC', 'CMOVNE', 'CMOVNG', 'CMOVNGE', 'CMOVNL',
  'CMOVNLE', 'CMOVNO', 'CMOVNP', 'CMOVNS', 'CMOVNZ', 'CMOVO', 'CMOVP',
  'CMOVPE', 'CMOVPO', 'CMOVS', 'CMOVZ', 'CMP', 'CMPPD', 'CMPPS', 'CMPS',
  'CMPSB', 'CMPSD', 'CMPSQ', 'CMPSS', 'CMPSW', 'CMPXCHG', 'CMPXCHG16B',
  'CMPXCHG8B', 'COMISD', 'COMISS', 'CPUID', 'CQO', 'CRC32', 'CVTDQ2PD',
  'CVTDQ2PS', 'CVTPD2DQ', 'CVTPD2PI', 'CVTPD2PS', 'CVTPI2PD', 'CVTPI2PS',
  'CVTPS2DQ', 'CVTPS2PD', 'CVTPS2PI', 'CVTSD2SI', 'CVTSD2SS', 'CVTSI2SD',
  'CVTSI2SS', 'CVTSS2SD', 'CVTSS2SI', 'CVTTPD2DQ', 'CVTTPD2PI', 'CVTTPS2DQ',
  'CVTTPS2PI', 'CVTTSD2SI', 'CVTTSS2SI', 'CWD', 'CWDE', 'DAA', 'DAS', 'DEC',
  'DIV', 'DIVPD', 'DIVPS', 'DIVSD', 'DIVSS', 'DPPD', 'DPPS', 'EMMS',
  'ENCLU', 'ENCODEKEY128', 'ENCODEKEY256', 'ENDBR32', 'ENDBR64', 'ENQCMD',
  'ENTER', 'ESC', 'EXTRACTPS', 'EXTRQ', 'F2XM1', 'FABS', 'FADD', 'FADDP',
  'FBLD', 'FBSTP', 'FCHS', 'FCLEX', 'FCMOVB', 'FCMOVBE', 'FCMOVCC', 'FCMOVE',
  'FCMOVNB', 'FCMOVNBE', 'FCMOVNE', 'FCMOVNU', 'FCMOVU', 'FCOM', 'FCOMI',
  'FCOMIP', 'FCOMP', 'FCOMPP', 'FCOS', 'FDECSTP', 'FDISI', 'FDIV', 'FDIVP',
  'FDIVR', 'FDIVRP', 'FENI', 'FFREE', 'FFREEP', 'FIADD', 'FICOM', 'FICOMP',
  'FIDIV', 'FIDIVR', 'FILD', 'FIMUL', 'FINCSTP', 'FINIT', 'FIST', 'FISTP',
  'FISTTP', 'FISUB', 'FISUBR', 'FLD', 'FLD1', 'FLDCW', 'FLDENV', 'FLDL2E',
  'FLDL2T', 'FLDLG2', 'FLDLN2', 'FLDPI', 'FLDZ', 'FMUL', 'FMULP', 'FNCLEX',
  'FNDISI', 'FNENI', 'FNINIT', 'FNSAVE', 'FNSETPM', 'FNSTCW', 'FNSTENV',
  'FNSTSW', 'FPATAN', 'FPREM', 'FPREM1', 'FPTAN', 'FRNDINT', 'FRSTOR',
  'FSAVE', 'FSCALE', 'FSETPM', 'FSIN', 'FSINCOS', 'FSQRT', 'FST', 'FSTCW',
  'FSTENV', 'FSTP', 'FSTPNCE', 'FSTSW', 'FSUB', 'FSUBP', 'FSUBR', 'FSUBRP',
  'FTST', 'FUCOM', 'FUCOMI', 'FUCOMIP', 'FUCOMP', 'FUCOMPP', 'FWAIT', 'FXAM',
  'FXCH', 'FXRSTOR', 'FXSAVE', 'FXTRACT', 'FYL2X', 'FYL2XP1', 'HADDPD',
  'HADDPS', 'HINT_NOP', 'HLT', 'HSUBPD', 'HSUBPS', 'IBTS', 'ICEBP', 'IDIV',
  'IMUL', 'INC', 'INCSSPD', 'INCSSPQ', 'INSERTPS', 'INSERTQ', 'IRET', 'IRETD',
  'IRETQ', 'KADDB', 'KADDD', 'KADDQ', 'KADDW',
  'KANDB', 'KANDD', 'KANDNB', 'KANDND', 'KANDNQ', 'KANDNW', 'KANDQ', 'KANDW',
  'KMOVB', 'KMOVD', 'KMOVQ', 'KMOVW', 'KNOTB', 'KNOTD', 'KNOTQ', 'KNOTW',
  'KORB', 'KORD', 'KORQ', 'KORTESTB', 'KORTESTD', 'KORTESTQ', 'KORTESTW',
  'KORW', 'KSHIFTLB', 'KSHIFTLD', 'KSHIFTLQ', 'KSHIFTLW', 'KSHIFTRB',
  'KSHIFTRD', 'KSHIFTRQ', 'KSHIFTRW', 'KTESTB', 'KTESTD', 'KTESTQ', 'KTESTW',
  'KUNPCKBW', 'KUNPCKDQ', 'KUNPCKWD', 'KXNORB', 'KXNORD', 'KXNORQ', 'KXNORW',
  'KXORB', 'KXORD', 'KXORQ', 'KXORW', 'LAHF', 'LDDQU', 'LDMXCSR', 'LDS', 'LEA',
  'LEAVE', 'LES', 'LFENCE', 'LFS', 'LGS', 'LOADIWKEY', 'LODS', 'LODSB',
  'LODSD', 'LODSQ', 'LODSW', 'LOOP', 'LOOPCC', 'LOOPE', 'LOOPNE', 'LOOPNZ',
  'LOOPZ', 'LSS', 'LZCNT', 'MASKMOVDQU', 'MASKMOVQ', 'MAXPD', 'MAXPS', 'MAXSD',
  'MAXSS', 'MCOMMIT', 'MFENCE', 'MINPD', 'MINPS', 'MINSD', 'MINSS', 'MONITORX',
  'MONTMUL', 'MOV', 'MOVABS', 'MOVAPD', 'MOVAPS', 'MOVBE', 'MOVD', 'MOVDDUP',
  'MOVDIR64B', 'MOVDIRI', 'MOVDQ2Q', 'MOVDQA', 'MOVDQU', 'MOVHLPS', 'MOVHPD',
  'MOVHPS', 'MOVLHPS', 'MOVLPD', 'MOVLPS', 'MOVMSKPD', 'MOVMSKPS', 'MOVNTDQ',
  'MOVNTDQA', 'MOVNTI', 'MOVNTPD', 'MOVNTPS', 'MOVNTQ', 'MOVNTSD', 'MOVNTSS',
  'MOVQ', 'MOVQ2DQ', 'MOVS', 'MOVSB', 'MOVSD', 'MOVSHDUP', 'MOVSLDUP', 'MOVSQ',
  'MOVSS', 'MOVSW', 'MOVSX', 'MOVSXD', 'MOVUPD', 'MOVUPS', 'MOVZX', 'MPSADBW',
  'MUL', 'MULPD', 'MULPS', 'MULSD', 'MULSS', 'MULX', 'MWAITX', 'NEG', 'NOT',
  'OIO', 'OR', 'ORPD', 'ORPS', 'PABSB', 'PABSD', 'PABSQ', 'PABSW', 'PACKSSDW',
  'PACKSSWB', 'PACKUSDW', 'PACKUSWB', 'PADDB', 'PADDD', 'PADDQ', 'PADDSB',
  'PADDSW', 'PADDUSB', 'PADDUSW', 'PADDW', 'PALIGNR', 'PAND', 'PANDN', 'PAUSE',
  'PAVGB', 'PAVGW', 'PBLENDVB', 'PBLENDW', 'PCLMULHQHQDQ', 'PCLMULHQLQDQ',
  'PCLMULLQHQDQ', 'PCLMULLQLQDQ', 'PCLMULQDQ', 'PCMPEQB', 'PCMPEQD', 'PCMPEQQ',
  'PCMPEQW', 'PCMPESTRI', 'PCMPESTRM', 'PCMPGTB', 'PCMPGTD', 'PCMPGTQ',
  'PCMPGTW', 'PCMPISTRI', 'PCMPISTRM', 'PDEP', 'PEXT', 'PEXTRB', 'PEXTRD',
  'PEXTRQ', 'PEXTRW', 'PHADDD', 'PHADDSW', 'PHADDW', 'PHMINPOSUW', 'PHSUBD',
  'PHSUBSW', 'PHSUBW', 'PINSRB', 'PINSRD', 'PINSRQ', 'PINSRW', 'PMADDUBSW',
  'PMADDWD', 'PMAXSB', 'PMAXSD', 'PMAXSQ', 'PMAXSW', 'PMAXUB', 'PMAXUD',
  'PMAXUQ', 'PMAXUW', 'PMINSB', 'PMINSD', 'PMINSQ', 'PMINSW', 'PMINUB',
  'PMINUD', 'PMINUQ', 'PMINUW', 'PMOVMSKB', 'PMOVSX', 'PMOVSXBD', 'PMOVSXBQ',
  'PMOVSXBW', 'PMOVSXDQ', 'PMOVSXWD', 'PMOVSXWQ', 'PMOVZX', 'PMOVZXBD',
  'PMOVZXBQ', 'PMOVZXBW', 'PMOVZXDQ', 'PMOVZXWD', 'PMOVZXWQ', 'PMULDQ',
  'PMULHRSW', 'PMULHUW', 'PMULHW', 'PMULLD', 'PMULLQ', 'PMULLW', 'PMULUDQ',
  'POP', 'POPA', 'POPAD', 'POPAL', 'POPCNT', 'POPF', 'POPFD', 'POPFQ', 'POR',
  'PREFETCH', 'PREFETCHH', 'PREFETCHNTA', 'PREFETCHT0', 'PREFETCHT1',
  'PREFETCHT2', 'PREFETCHW', 'PREFETCHWT1', 'PROLD', 'PROLQ', 'PROLVD',
  'PROLVQ', 'PRORD', 'PRORQ', 'PRORVD', 'PRORVQ', 'PSADBW', 'PSHUFB', 'PSHUFD',
  'PSHUFHW', 'PSHUFLW', 'PSHUFW', 'PSIGNB', 'PSIGND', 'PSIGNW', 'PSLLD',
  'PSLLDQ', 'PSLLQ', 'PSLLW', 'PSRAD', 'PSRAQ', 'PSRAW', 'PSRLD', 'PSRLDQ',
  'PSRLQ', 'PSRLW', 'PSUBB', 'PSUBD', 'PSUBQ', 'PSUBSB', 'PSUBSW', 'PSUBUSB',
  'PSUBUSW', 'PSUBW', 'PTEST', 'PTR', 'PTWRITE', 'PUNPCKHBW', 'PUNPCKHDQ',
  'PUNPCKHQDQ', 'PUNPCKHWD', 'PUNPCKLBW', 'PUNPCKLDQ', 'PUNPCKLQDQ',
  'PUNPCKLWD', 'PUSH', 'PUSHA', 'PUSHAD', 'PUSHAL', 'PUSHF', 'PUSHFD',
  'PUSHFQ', 'PXOR', 'RCL', 'RCPPS', 'RCPSS', 'RCR', 'RDFSBASE', 'RDGSBASE',
  'RDPID', 'RDPKRU', 'RDPMC', 'RDPRU', 'RDRAND', 'RDSEED', 'RDSSP', 'RDSSPD',
  'RDSSPQ', 'RDTSC', 'RDTSCP', 'RETF', 'RETN', 'ROL', 'ROR', 'RORX',
  'ROUNDPD', 'ROUNDPS', 'ROUNDSD', 'ROUNDSS', 'RSQRTPS', 'RSQRTSS', 'RSTORSSP',
  'RTDSC', 'SAHF', 'SAL', 'SALC', 'SAR', 'SARX', 'SAVEPREVSSP', 'SBB', 'SCAS',
  'SCASB', 'SCASD', 'SCASQ', 'SCASW', 'SENDUIPI', 'SERIALIZE', 'SETA', 'SETAE',
  'SETALC', 'SETB', 'SETBE', 'SETC', 'SETCC', 'SETE', 'SETG', 'SETGE', 'SETL',
  'SETLE', 'SETNA', 'SETNAE', 'SETNB', 'SETNBE', 'SETNC', 'SETNE', 'SETNG',
  'SETNGE', 'SETNL', 'SETNLE', 'SETNO', 'SETNP', 'SETNS', 'SETNZ', 'SETO',
  'SETP', 'SETPE', 'SETPO', 'SETS', 'SETZ', 'SFENCE', 'SHA1MSG1', 'SHA1MSG2',
  'SHA1NEXTE', 'SHA1RNDS4', 'SHA256MSG1', 'SHA256MSG2', 'SHA256RNDS2', 'SHL',
  'SHLD', 'SHLX', 'SHR', 'SHRD', 'SHRX', 'SHUFPD', 'SHUFPS', 'SQRTPD',
  'SQRTPS', 'SQRTSD', 'SQRTSS', 'STC', 'STD', 'STI', 'STMXCSR', 'STOS',
  'STOSB', 'STOSD', 'STOSQ', 'STOSW', 'STUI', 'SUB', 'SUBPD', 'SUBPS', 'SUBSD',
  'SUBSS', 'TEST', 'TESTUI', 'TPAUSE', 'TZCNT', 'UCOMISD', 'UCOMISS', 'UD0',
  'UD1', 'UD2', 'UD2A', 'UD2B', 'UIRET', 'UMONITOR', 'UMOV', 'UMWAIT',
  'UNPCKHPD', 'UNPCKHPS', 'UNPCKLPD', 'UNPCKLPS', 'VADDPD', 'VADDPS', 'VADDSD',
  'VADDSS', 'VADDSUBPD', 'VADDSUBPS', 'VAESDEC', 'VAESDECLAST', 'VAESENC',
  'VAESENCLAST', 'VAESIMC', 'VAESKEYGENASSIST', 'VALIGND', 'VALIGNQ',
  'VANDNPD', 'VANDNPS', 'VANDPD', 'VANDPS', 'VBLENDMPD', 'VBLENDMPS',
  'VBLENDPD', 'VBLENDPS', 'VBLENDVPD', 'VBLENDVPS', 'VBROADCAST',
  'VBROADCASTF128', 'VBROADCASTF32X2', 'VBROADCASTF32X4', 'VBROADCASTF32X8',
  'VBROADCASTF64X2', 'VBROADCASTF64X4', 'VBROADCASTI128', 'VBROADCASTI32X2',
  'VBROADCASTI32X8', 'VBROADCASTI64X4', 'VBROADCASTSD', 'VBROADCASTSS',
  'VCMPPD', 'VCMPPS', 'VCMPSD', 'VCMPSS', 'VCOMISD', 'VCOMISS', 'VCOMPRESSPD',
  'VCOMPRESSPS', 'VCVTDQ2PD', 'VCVTDQ2PS', 'VCVTPD2DQ', 'VCVTPD2PS',
  'VCVTPD2QQ', 'VCVTPD2UDQ', 'VCVTPD2UQQ', 'VCVTPH2PS', 'VCVTPS2DQ',
  'VCVTPS2PD', 'VCVTPS2PH', 'VCVTPS2QQ', 'VCVTPS2UDQ', 'VCVTPS2UQQ',
  'VCVTQQ2PD', 'VCVTQQ2PS', 'VCVTSD2SI', 'VCVTSD2SS', 'VCVTSD2USI',
  'VCVTSI2SD', 'VCVTSI2SS', 'VCVTSS2SD', 'VCVTSS2SI', 'VCVTSS2USI',
  'VCVTTPD2DQ', 'VCVTTPD2QQ', 'VCVTTPD2UDQ', 'VCVTTPD2UQQ', 'VCVTTPS2DQ',
  'VCVTTPS2QQ', 'VCVTTPS2UDQ', 'VCVTTPS2UQQ', 'VCVTTSD2SI', 'VCVTTSD2USI',
  'VCVTTSS2SI', 'VCVTTSS2USI', 'VCVTUDQ2PD', 'VCVTUDQ2PS', 'VCVTUQQ2PD',
  'VCVTUQQ2PS', 'VCVTUSI2SD', 'VCVTUSI2SS', 'VDBPSADBW', 'VDIVPD', 'VDIVPS',
  'VDIVSD', 'VDIVSS', 'VDPPD', 'VDPPS', 'VEXP2PD', 'VEXP2PS', 'VEXPANDPD',
  'VEXPANDPS', 'VEXTRACTF128', 'VEXTRACTF32X4', 'VEXTRACTF32X8',
  'VEXTRACTF64X2', 'VEXTRACTF64X4', 'VEXTRACTI128', 'VEXTRACTI32X4',
  'VEXTRACTI32X8', 'VEXTRACTI64X2', 'VEXTRACTI64X4', 'VEXTRACTPS',
  'VFIXUPIMMPD', 'VFIXUPIMMPS', 'VFIXUPIMMSD', 'VFIXUPIMMSS', 'VFMADD132PD',
  'VFMADD132PS', 'VFMADD132SD', 'VFMADD132SS', 'VFMADD213PD', 'VFMADD213PS',
  'VFMADD213SD', 'VFMADD213SS', 'VFMADD231PD', 'VFMADD231PS', 'VFMADD231SD',
  'VFMADD231SS', 'VFMADDSUB132PD', 'VFMADDSUB132PS', 'VFMADDSUB213PD',
  'VFMADDSUB213PS', 'VFMADDSUB231PD', 'VFMADDSUB231PS', 'VFMSUB132PD',
  'VFMSUB132PS', 'VFMSUB132SD', 'VFMSUB132SS', 'VFMSUB213PD', 'VFMSUB213PS',
  'VFMSUB213SD', 'VFMSUB213SS', 'VFMSUB231PD', 'VFMSUB231PS', 'VFMSUB231SD',
  'VFMSUB231SS', 'VFMSUBADD132PD', 'VFMSUBADD132PS', 'VFMSUBADD213PD',
  'VFMSUBADD213PS', 'VFMSUBADD231PD', 'VFMSUBADD231PS', 'VFNMADD132PD',
  'VFNMADD132PS', 'VFNMADD132SD', 'VFNMADD132SS', 'VFNMADD213PD',
  'VFNMADD213PS', 'VFNMADD213SD', 'VFNMADD213SS', 'VFNMADD231PD',
  'VFNMADD231PS', 'VFNMADD231SD', 'VFNMADD231SS', 'VFNMSUB132PD',
  'VFNMSUB132PS', 'VFNMSUB132SD', 'VFNMSUB132SS', 'VFNMSUB213PD',
  'VFNMSUB213PS', 'VFNMSUB213SD', 'VFNMSUB213SS', 'VFNMSUB231PD',
  'VFNMSUB231PS', 'VFNMSUB231SD', 'VFNMSUB231SS', 'VFPCLASSPD', 'VFPCLASSPS',
  'VFPCLASSSD', 'VFPCLASSSS', 'VGATHERDPD', 'VGATHERDPS', 'VGATHERPF0DPD',
  'VGATHERPF0DPS', 'VGATHERPF0QPD', 'VGATHERPF0QPS', 'VGATHERPF1DPD',
  'VGATHERPF1DPS', 'VGATHERPF1QPD', 'VGATHERPF1QPS', 'VGATHERQPD',
  'VGATHERQPS', 'VGETEXPPD', 'VGETEXPPS', 'VGETEXPSD', 'VGETEXPSS',
  'VGETMANTPD', 'VGETMANTPS', 'VGETMANTSD', 'VGETMANTSS', 'VHADDPD', 'VHADDPS',
  'VHSUBPD', 'VHSUBPS', 'VINSERTF128', 'VINSERTF32X4', 'VINSERTF32X8',
  'VINSERTF64X2', 'VINSERTF64X4', 'VINSERTI128', 'VINSERTI32X4',
  'VINSERTI32X8', 'VINSERTI64X2', 'VINSERTI64X4', 'VINSERTPS', 'VLDDQU',
  'VLDMXCSR', 'VMASKMOV', 'VMASKMOVDQU', 'VMASKMOVPD', 'VMASKMOVPS', 'VMAXPD',
  'VMAXPS', 'VMAXSD', 'VMAXSS', 'VMINPD', 'VMINPS', 'VMINSD', 'VMINSS',
  'VMOVAPD', 'VMOVAPS', 'VMOVD', 'VMOVDDUP', 'VMOVDQA', 'VMOVDQA32',
  'VMOVDQA64', 'VMOVDQU', 'VMOVDQU16', 'VMOVDQU32', 'VMOVDQU64', 'VMOVDQU8',
  'VMOVHLPS', 'VMOVHPD', 'VMOVHPS', 'VMOVLHPS', 'VMOVLPD', 'VMOVLPS',
  'VMOVMSKPD', 'VMOVMSKPS', 'VMOVNTDQ', 'VMOVNTDQA', 'VMOVNTPD', 'VMOVNTPS',
  'VMOVQ', 'VMOVSD', 'VMOVSHDUP', 'VMOVSLDUP', 'VMOVSS', 'VMOVUPD', 'VMOVUPS',
  'VMPSADBW', 'VMULPD', 'VMULPS', 'VMULSD', 'VMULSS', 'VORPD', 'VORPS',
  'VPABSB', 'VPABSD', 'VPABSQ', 'VPABSW', 'VPACKSSDW', 'VPACKSSWB',
  'VPACKUSDW', 'VPACKUSWB', 'VPADDB', 'VPADDD', 'VPADDQ', 'VPADDSB', 'VPADDSW',
  'VPADDUSB', 'VPADDUSW', 'VPADDW', 'VPALIGNR', 'VPAND', 'VPANDD', 'VPANDN',
  'VPANDND', 'VPANDNQ', 'VPANDQ', 'VPAVGB', 'VPAVGW', 'VPBLENDD', 'VPBLENDMB',
  'VPBLENDMD', 'VPBLENDMQ', 'VPBLENDMW', 'VPBLENDVB', 'VPBLENDW',
  'VPBROADCAST', 'VPBROADCASTB', 'VPBROADCASTD', 'VPBROADCASTM',
  'VPBROADCASTMB2Q', 'VPBROADCASTMW2D', 'VPBROADCASTQ', 'VPBROADCASTW',
  'VPCLMULQDQ', 'VPCMPB', 'VPCMPD', 'VPCMPEQB', 'VPCMPEQD', 'VPCMPEQQ',
  'VPCMPEQW', 'VPCMPESTRI', 'VPCMPESTRM', 'VPCMPGTB', 'VPCMPGTD', 'VPCMPGTQ',
  'VPCMPGTW', 'VPCMPISTRI', 'VPCMPISTRM', 'VPCMPQ', 'VPCMPUB', 'VPCMPUD',
  'VPCMPUQ', 'VPCMPUW', 'VPCMPW', 'VPCOMPRESSD', 'VPCOMPRESSQ', 'VPCONFLICTD',
  'VPCONFLICTQ', 'VPERM2F128', 'VPERM2I128', 'VPERMD', 'VPERMI2D', 'VPERMI2PD',
  'VPERMI2PS', 'VPERMI2Q', 'VPERMI2W', 'VPERMILPD', 'VPERMILPS', 'VPERMPD',
  'VPERMPS', 'VPERMQ', 'VPERMW', 'VPEXPANDD', 'VPEXPANDQ', 'VPEXTRB',
  'VPEXTRD', 'VPEXTRQ', 'VPEXTRW', 'VPGATHERDD', 'VPGATHERDQ', 'VPGATHERQD',
  'VPGATHERQQ', 'VPHADDD', 'VPHADDSW', 'VPHADDW', 'VPHMINPOSUW', 'VPHSUBD',
  'VPHSUBSW', 'VPHSUBW', 'VPINSRB', 'VPINSRD', 'VPINSRQ', 'VPINSRW',
  'VPLZCNTD', 'VPLZCNTQ', 'VPMADDUBSW', 'VPMADDWD', 'VPMASKMOV', 'VPMASKMOVD',
  'VPMASKMOVQ', 'VPMAXSB', 'VPMAXSD', 'VPMAXSQ', 'VPMAXSW', 'VPMAXUB',
  'VPMAXUD', 'VPMAXUQ', 'VPMAXUW', 'VPMINSB', 'VPMINSD', 'VPMINSQ', 'VPMINSW',
  'VPMINUB', 'VPMINUD', 'VPMINUQ', 'VPMINUW', 'VPMOVB2M', 'VPMOVD2M',
  'VPMOVDB', 'VPMOVDW', 'VPMOVM2B', 'VPMOVM2D', 'VPMOVM2Q', 'VPMOVM2W',
  'VPMOVMSKB', 'VPMOVQ2M', 'VPMOVQB', 'VPMOVQD', 'VPMOVQW', 'VPMOVSDB',
  'VPMOVSDW', 'VPMOVSQB', 'VPMOVSQD', 'VPMOVSQW', 'VPMOVSWB', 'VPMOVSXBD',
  'VPMOVSXBQ', 'VPMOVSXBW', 'VPMOVSXDQ', 'VPMOVSXWD', 'VPMOVSXWQ', 'VPMOVUSDB',
  'VPMOVUSDW', 'VPMOVUSQB', 'VPMOVUSQD', 'VPMOVUSQW', 'VPMOVUSWB', 'VPMOVW2M',
  'VPMOVWB', 'VPMOVZXBD', 'VPMOVZXBQ', 'VPMOVZXBW', 'VPMOVZXDQ', 'VPMOVZXWD',
  'VPMOVZXWQ', 'VPMULDQ', 'VPMULHRSW', 'VPMULHUW', 'VPMULHW', 'VPMULLD',
  'VPMULLQ', 'VPMULLW', 'VPMULUDQ', 'VPOR', 'VPORD', 'VPORQ', 'VPROLD',
  'VPROLQ', 'VPROLVD', 'VPROLVQ', 'VPRORD', 'VPRORQ', 'VPRORVD', 'VPRORVQ',
  'VPSADBW', 'VPSCATTERDD', 'VPSCATTERDQ', 'VPSCATTERQD', 'VPSCATTERQQ',
  'VPSHUFB', 'VPSHUFD', 'VPSHUFHW', 'VPSHUFLW', 'VPSIGNB', 'VPSIGND',
  'VPSIGNW', 'VPSLLD', 'VPSLLDQ', 'VPSLLQ', 'VPSLLVD', 'VPSLLVQ', 'VPSLLVW',
  'VPSLLW', 'VPSRAD', 'VPSRAQ', 'VPSRAVD', 'VPSRAVQ', 'VPSRAVW', 'VPSRAW',
  'VPSRLD', 'VPSRLDQ', 'VPSRLQ', 'VPSRLVD', 'VPSRLVQ', 'VPSRLVW', 'VPSRLW',
  'VPSUBB', 'VPSUBD', 'VPSUBQ', 'VPSUBSB', 'VPSUBSW', 'VPSUBUSB', 'VPSUBUSW',
  'VPSUBW', 'VPTERNLOGD', 'VPTERNLOGQ', 'VPTEST', 'VPTESTMB', 'VPTESTMD',
  'VPTESTMQ', 'VPTESTMW', 'VPTESTNMB', 'VPTESTNMD', 'VPTESTNMQ', 'VPTESTNMW',
  'VPUNPCKHBW', 'VPUNPCKHDQ', 'VPUNPCKHQDQ', 'VPUNPCKHWD', 'VPUNPCKLBW',
  'VPUNPCKLDQ', 'VPUNPCKLQDQ', 'VPUNPCKLWD', 'VPXOR', 'VPXORD', 'VPXORQ',
  'VRANGEPD', 'VRANGEPS', 'VRANGESD', 'VRANGESS', 'VRCP14PD', 'VRCP14PS',
  'VRCP14SD', 'VRCP14SS', 'VRCP28PD', 'VRCP28PS', 'VRCP28SD', 'VRCP28SS',
  'VRCPPS', 'VRCPSS', 'VREDUCEPD', 'VREDUCEPS', 'VREDUCESD', 'VREDUCESS',
  'VRNDSCALEPD', 'VRNDSCALEPS', 'VRNDSCALESD', 'VRNDSCALESS', 'VROUNDPD',
  'VROUNDPS', 'VROUNDSD', 'VROUNDSS', 'VRSQRT14PD', 'VRSQRT14PS', 'VRSQRT14SD',
  'VRSQRT14SS', 'VRSQRT28PD', 'VRSQRT28PS', 'VRSQRT28SD', 'VRSQRT28SS',
  'VRSQRTPS', 'VRSQRTSS', 'VSCALEFPD', 'VSCALEFPS', 'VSCALEFSD', 'VSCALEFSS',
  'VSCATTERDPD', 'VSCATTERDPS', 'VSCATTERPF0DPD', 'VSCATTERPF0DPS',
  'VSCATTERPF0QPD', 'VSCATTERPF0QPS', 'VSCATTERPF1DPD', 'VSCATTERPF1DPS',
  'VSCATTERPF1QPD', 'VSCATTERPF1QPS', 'VSCATTERQPD', 'VSCATTERQPS',
  'VSHUFF32X4', 'VSHUFF64X2', 'VSHUFI32X4', 'VSHUFI64X2', 'VSHUFPD', 'VSHUFPS',
  'VSQRTPD', 'VSQRTPS', 'VSQRTSD', 'VSQRTSS', 'VSTMXCSR', 'VSUBPD', 'VSUBPS',
  'VSUBSD', 'VSUBSS', 'VTESTPD', 'VTESTPS', 'VUCOMISD', 'VUCOMISS',
  'VUNPCKHPD', 'VUNPCKHPS', 'VUNPCKLPD', 'VUNPCKLPS', 'VXORPD', 'VXORPS',
  'VZEROALL', 'VZEROUPPER', 'WAIT', 'WRFSBASE', 'WRGSBASE', 'WRPKRU', 'WRSSD',
  'WRSSQ', 'XABORT', 'XACQUIRE', 'XADD', 'XBEGIN', 'XBTS', 'XCHG', 'XCRYPTCBC',
  'XCRYPTCFB', 'XCRYPTCTR', 'XCRYPTECB', 'XCRYPTOFB', 'XEND', 'XGETBV', 'XLAT',
  'XLATB', 'XOR', 'XORPD', 'XORPS', 'XRELEASE', 'XRESLDTRK', 'XRSTOR',
  'XRSTOR64', 'XSAVE', 'XSAVE64', 'XSAVEC', 'XSAVEC64', 'XSAVEOPT',
  'XSAVEOPT64', 'XSHA1', 'XSHA256', 'XSTORE', 'XSUSLDTRK', 'XTEST'
  );

const SizeBuff: array[0..7] of string = (
  'BYTE', 'WORD', 'DWORD', 'QWORD', 'TBYTE', 'XMMWORD', 'YMMWORD', 'ZMMWORD');

const JmpBuff: array[0..38] of string = (
  'CALL', 'JA', 'JAE', 'JB', 'JBE', 'JC', 'JCXZ', 'JE', 'JECXZ', 'JG', 'JGE', 'JL',
  'JLE', 'JMP', 'JNA', 'JNAE', 'JNB', 'JNBE', 'JNC', 'JNE', 'JNG', 'JNGE', 'JNL', 'JNLE',
  'JNO', 'JNP', 'JNS', 'JNZ', 'JO', 'JP', 'JPE', 'JPO', 'JS', 'JZ',
  'JCC', 'JMPE', 'JMPF', 'JRCXZ', 'RET'
  );

const KernelBuff: array[0..100] of string = (
  'ARPL', 'CLAC', 'CLGI', 'CLRSSBSY', 'CLTS', 'DB', 'ENCLS', 'ENCLV', 'ENQCMDS', 'GETSEC',
  'HRESET', 'IN', 'INS', 'INSB', 'INSW', 'INSD', 'INT', 'INT01', 'INT1', 'INT3', 'INTO',
  'INVD', 'INVEPT', 'INVLPG', 'INVLPGA', 'INVLPGB', 'INVPCID', 'INVVPID',
  'LAR', 'LGDT', 'LIDT', 'LLDT', 'LMSW', 'LOADALL', 'LOADALL386', 'LOADALLD', 'LSL',
  'LTR', 'MONITOR', 'MWAIT', 'OUT', 'OUTS', 'OUTSB', 'OUTSW', 'OUTSD', 'PCONFIG', 'PSMASH',
  'PVALIDATE', 'RDMSR', 'RMPADJUST', 'RMPQUERY', 'RMPUPDATE', 'RSM', 'SEAMCALL', 'SEAMOPS',
  'SEAMRET', 'SETSSBSY', 'SGDT', 'SIDT', 'SKINIT', 'SLDT', 'SMSW', 'STAC', 'STGI', 'STOREALL',
  'STR', 'SWAPGS', 'SYSCALL', 'SYSENTER', 'SYSEXIT', 'SYSRET', 'TDCALL', 'TLBSYNC', 'VERR',
  'VERW', 'VMCALL', 'VMCLEAR', 'VMFUNC', 'VMGEXIT', 'VMLAUNCH', 'VMLOAD', 'VMMCALL', 'VMPTRLD',
  'VMPTRST', 'VMREAD', 'VMRESUME', 'VMRUN', 'VMSAVE', 'VMWRITE', 'VMXOFF', 'VMXON', 'WBINVD',
  'WBNOINVD', 'WRMSR', 'WRUSSD', 'WRUSSQ', 'XRSTORS', 'XRSTORS64', 'XSAVES', 'XSAVES64', 'XSETBV'
  );

const PrefixBuff: array[0..22] of string = (
  'LOCK', 'NOTRACK', 'REP', 'REPE', 'REPNE', 'REPNZ', 'REPZ',
  'REX', 'REX.B', 'REX.R', 'REX.RB', 'REX.RX', 'REX.RXB', 'REX.W', 'REX.WB',
  'REX.WR', 'REX.WRB', 'REX.WRX','REX.WRXB', 'REX.WX', 'REX.WXB', 'REX.X', 'REX.XB'
  );

const RegsBuff: array[0..137] of string = (
  'AH', 'AL', 'AX', 'BH', 'BL', 'BP', 'BPL', 'BX', 'CH', 'CL', 'CR0', 'CR2', 'CR3',
  'CR4', 'CR8', 'CS', 'CX', 'DH', 'DI', 'DIL', 'DL', 'DR0', 'DR1', 'DR2', 'DR3', 'DR6',
  'DR7', 'DS', 'DX', 'EAX', 'EBP', 'EBX', 'ECX', 'EDI', 'EDX', 'EIP', 'ES', 'ESI', 'ESP',
  'FS', 'GS', 'MM0', 'MM1', 'MM2', 'MM3', 'MM4', 'MM5', 'MM6', 'MM7', 'R10', 'R10B',
  'R10D', 'R10W', 'R11', 'R11B', 'R11D', 'R11W', 'R12', 'R12B', 'R12D', 'R12W', 'R13',
  'R13B', 'R13D', 'R13W', 'R14', 'R14B', 'R14D', 'R14W', 'R15', 'R15B', 'R15D', 'R15W',
  'R8', 'R8B', 'R8D', 'R8W', 'R9', 'R9B', 'R9D', 'R9W', 'RAX', 'RBP', 'RBX', 'RCX',
  'RDI', 'RDX', 'RIP', 'RSI', 'RSP', 'SI', 'SIL', 'SP', 'SPL', 'SS', 'ST(0)', 'ST(1)',
  'ST(2)', 'ST(3)', 'ST(4)', 'ST(5)', 'ST(6)', 'ST(7)', 'XMM0', 'XMM1', 'XMM10', 'XMM11', 'XMM12',
  'XMM13', 'XMM14', 'XMM15', 'XMM2', 'XMM3', 'XMM4', 'XMM5', 'XMM6', 'XMM7', 'XMM8',
  'XMM9', 'YMM0', 'YMM1', 'YMM10', 'YMM11', 'YMM12', 'YMM13', 'YMM14', 'YMM15', 'YMM2',
  'YMM3', 'YMM4', 'YMM5', 'YMM6', 'YMM7', 'YMM8', 'YMM9',
  'RFLAGS', 'EFLAGS', 'FLAGS'
  );

const RegTypesBuff: array[0..137] of TRegType = (
  // 'AH',   'AL',   'AX',    'BH',   'BL',   'BP',    'BPL',  'BX',   'CH',    'CL',
     rtReg8, rtReg8, rtReg16, rtReg8, rtReg8, rtReg16, rtReg8, rtReg16, rtReg8, rtReg8,
  // 'CR0',  'CR2',    'CR3',   'CR4',   'CR8',
     rtReg32, rtReg32, rtReg32, rtReg32, rtReg32,
  // 'CS',     'CX',    'DH',   'DI',    'DIL',  'DL',
     rtRegSeg, rtReg16, rtReg8, rtReg16, rtReg8, rtReg8,
  // 'DR0',    'DR1',    'DR2',    'DR3',    'DR6',    'DR7',
     rtRegPtr, rtRegPtr, rtRegPtr, rtRegPtr, rtRegPtr, rtRegPtr,
  // 'DS',     'DX',    'EAX',   'EBP',   'EBX',   'ECX',   'EDI',   'EDX',   'EIP',
     rtRegSeg, rtReg16, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32,
  //  'ES',    'ESI',   'ESP',   'FS',     'GS',     'MM0',    'MM1',    'MM2',
     rtRegSeg, rtReg32, rtReg32, rtRegSeg, rtRegSeg, rtSimd64, rtSimd64, rtSimd64,
  // 'MM3',    'MM4',    'MM5',    'MM6',    'MM7',    'R10',   'R10B', 'R10D',
     rtSimd64, rtSimd64, rtSimd64, rtSimd64, rtSimd64, rtReg64, rtReg8, rtReg32,
  // 'R10W',  'R11',   'R11B', 'R11D',  'R11W',  'R12',   'R12B', 'R12D',  'R12W',
     rtReg16, rtReg64, rtReg8, rtReg32, rtReg16, rtReg64, rtReg8, rtReg32, rtReg16,
  // 'R13',   'R13B', 'R13D',  'R13W',  'R14',   'R14B', 'R14D',  'R14W',
     rtReg64, rtReg8, rtReg32, rtReg16, rtReg64, rtReg8, rtReg32, rtReg16,
  // 'R15',   'R15B', 'R15D',  'R15W',  'R8',    'R8B',  'R8D',   'R8W',
     rtReg64, rtReg8, rtReg32, rtReg16, rtReg64, rtReg8, rtReg32, rtReg16,
  // 'R9',    'R9B',  'R9D',   'R9W',   'RAX',   'RBP',   'RBX',   'RCX',
     rtReg64, rtReg8, rtReg32, rtReg16, rtReg64, rtReg64, rtReg64, rtReg64,
  // 'RDI',   'RDX',   'RIP',   'RSI',   'RSP',   'SI',    'SIL',  'SP',    'SPL',
     rtReg64, rtReg64, rtReg64, rtReg64, rtReg64, rtReg16, rtReg8, rtReg16, rtReg8,
  // 'SS',     'ST0', 'ST1', 'ST2', 'ST3', 'ST4', 'ST5', 'ST6', 'ST7',
     rtRegSeg, rtX87, rtX87, rtX87, rtX87, rtX87, rtX87, rtX87, rtX87,
  // 'XMM0',    'XMM1',    'XMM10',   'XMM11',   'XMM12',   'XMM13',   'XMM14',
     rtSimd128, rtSimd128, rtSimd128, rtSimd128, rtSimd128, rtSimd128, rtSimd128,
  // 'XMM15',   'XMM2',    'XMM3',    'XMM4',    'XMM5',    'XMM6',    'XMM7',
     rtSimd128, rtSimd128, rtSimd128, rtSimd128, rtSimd128, rtSimd128, rtSimd128,
  // 'XMM8',    'XMM9',    'YMM0',    'YMM1',    'YMM10',   'YMM11',   'YMM12',
     rtSimd128, rtSimd128, rtSimd256, rtSimd256, rtSimd256, rtSimd256, rtSimd256,
  // 'YMM13',   'YMM14',   'YMM15',   'YMM2',    'YMM3',    'YMM4',    'YMM5',
     rtSimd256, rtSimd256, rtSimd256, rtSimd256, rtSimd256, rtSimd256, rtSimd256,
  // 'YMM6',    'YMM7',    'YMM8',    'YMM9'
     rtSimd256, rtSimd256, rtSimd256, rtSimd256,
  // 'RFLAGS','EFLAGS','FLAGS'
     rtReg64, rtReg32, rtReg16
  );

const NopBuff: array[0..3] of string = (
  'FNOP', 'NOP', 'NOPL', 'NOPW'
  );

// AT&T specific

  // https://docs.oracle.com/cd/E19120-01/open.solaris/817-5477/enmzx/index.html

const AtAndTInstBuff: array[0..66] of string = (
  'ADDB', 'ADDW', 'ADDL', 'ADDQ',
  'ANDB', 'ANDW', 'ANDL', 'ANDQ',
  'CLTD',
  'CMOVW', 'CMOVQ',
  'CMPB', 'CMPW', 'CMPL', 'CMPQ',
  'CQTO',
  'DECB', 'DECW', 'DECL', 'DECQ',
  'DIVB', 'DIVW', 'DIVL', 'DIVQ',
  'FLDT',
  'IDIVB', 'IDIVW', 'IDIVL', 'IDIVQ',
  'IMULB', 'IMULW', 'IMULL', 'IMULQ',
  'INCB', 'INCW', 'INCL', 'INCQ',
  'LEAB', 'LEAW', 'LEAL', 'LEAQ',
  'MOVB', 'MOVW', 'MOVL',
  'MOVSBB', 'MOVSBW', 'MOVSBL', 'MOVSBQ',
  'MOVSLB', 'MOVSLW', 'MOVSLQ',
  'MOVSWB', 'MOVSWW', 'MOVSWL', 'MOVSWQ',
  'MOVZBB', 'MOVZBW', 'MOVZBL', 'MOVZBQ',
  'ORB', 'ORW', 'ORL', 'ORQ',
  'TESTB', 'TESTW', 'TESTL', 'TESTQ'
  );

const AtAndTJmpBuff: array[0..2] of string = (
  'CALLQ', 'JMPQ', 'RETQ'
  );

const AtAndTPrefixBuff: array[0..0] of string = (
  'DATA16'
  );

const ArmInstBuff: array [0..1076] of string = (
  'ABS', 'ADC', 'ADCS', 'ADD', 'ADDG', 'ADDHN', 'ADDHN2', 'ADDP', 'ADDS',
  'ADDV', 'ADR', 'ADRP', 'AESD', 'AESE', 'AESIMC', 'AESMC', 'AND', 'ANDS',
  'ASR', 'ASRV', 'AUTDA', 'AUTDB', 'AUTDZA', 'AUTDZB', 'AUTIA',
  'AUTIA1716', 'AUTIASP', 'AUTIAZ', 'AUTIB', 'AUTIB1716', 'AUTIBSP', 'AUTIBZ',
  'AUTIZA', 'AUTIZB', 'BCAX', 'BFC', 'BFI', 'BFM', 'BFXIL', 'BIC', 'BICS',
  'BIF', 'BIT', 'BKPT', 'BRK', 'BSL', 'CAS', 'CASA', 'CASAB', 'CASAH', 'CASAL',
  'CASALB', 'CASALH', 'CASB', 'CASH', 'CASL', 'CASLB', 'CASLH', 'CASP',
  'CASPA', 'CASPAL', 'CCMN', 'CCMP', 'CDP', 'CDP2', 'CINC',
  'CINV', 'CLREX', 'CLS', 'CLZ', 'CMEQ', 'CMGE', 'CMGT', 'CMHI', 'CMHS',
  'CMLE', 'CMLT', 'CMN', 'CMP', 'CMPP', 'CMTST', 'CNEG', 'CNT', 'CPS', 'CRC32',
  'CRC32B', 'CRC32C', 'CRC32CB', 'CRC32CH', 'CRC32CW', 'CRC32CX', 'CRC32H',
  'CRC32W', 'CRC32X', 'CSDB', 'CSEL', 'CSET', 'CSETM', 'CSINC', 'CSINV',
  'CSNEG', 'DBG', 'DCPS1', 'DCPS2', 'DCPS3', 'DMB', 'DRPS', 'DSB', 'DUP',
  'EON', 'EOR', 'EOR3', 'EORS', 'ESB', 'EXT', 'EXTR', 'FABD', 'FABS', 'FACGE',
  'FACGT', 'FADD', 'FADDP', 'FCADD', 'FCCMP', 'FCCMPE', 'FCMEQ', 'FCMGE',
  'FCMGT', 'FCMLA', 'FCMLE', 'FCMLT', 'FCMP', 'FCMPE', 'FCSEL', 'FCVT',
  'FCVTAS', 'FCVTAU', 'FCVTL', 'FCVTL2', 'FCVTMS', 'FCVTMU', 'FCVTN', 'FCVTN2',
  'FCVTNS', 'FCVTNU', 'FCVTPS', 'FCVTPU', 'FCVTXN', 'FCVTXN2', 'FCVTZS',
  'FCVTZU', 'FDIV', 'FJCVTZS', 'FLDMDBX', 'FLDMIAX', 'FMADD', 'FMAX', 'FMAXNM',
  'FMAXNMP', 'FMAXNMV', 'FMAXP', 'FMAXV', 'FMIN', 'FMINNM', 'FMINNMP',
  'FMINNMV', 'FMINP', 'FMINV', 'FMLA', 'FMLAL', 'FMLS', 'FMLSL', 'FMLSL2',
  'FMOV', 'FMSUB', 'FMUL', 'FMULX', 'FNEG', 'FNMADD', 'FNMSUB', 'FNMUL',
  'FRECPE', 'FRECPS', 'FRECPX', 'FRINTA', 'FRINTI', 'FRINTM', 'FRINTN',
  'FRINTP', 'FRINTX', 'FRINTZ', 'FRSQRTE', 'FRSQRTS', 'FSQRT', 'FSTMDBX',
  'FSTMIAX', 'FSUB', 'GMI', 'HINT', 'HLT', 'HVC', 'INS', 'IRG', 'ISB',
  'IT', 'ITE', 'ITEE', 'ITEEE', 'ITEET', 'ITET', 'ITETE', 'ITETT', 'ITT',
  'ITTE', 'ITTEE', 'ITTET', 'ITTT', 'ITTTE', 'ITTTT', 'LD1', 'LD1R', 'LD2',
  'LD2R', 'LD3', 'LD3R', 'LD4', 'LD4R', 'LDA', 'LDADD', 'LDADDA', 'LDADDAB',
  'LDADDAH', 'LDADDAL', 'LDADDALB', 'LDADDALH', 'LDADDB', 'LDADDH', 'LDADDL',
  'LDADDLB', 'LDADDLH', 'LDAEX', 'LDAEXB', 'LDAEXD', 'LDAEXH', 'LDAPR',
  'LDAPRB', 'LDAPRH', 'LDAR', 'LDARB', 'LDARH', 'LDAXP', 'LDAXR', 'LDAXRB',
  'LDAXRH', 'LDC', 'LDC2', 'LDC2L', 'LDCL', 'LDCLR', 'LDCLRA', 'LDCLRAB',
  'LDCLRAH', 'LDCLRAL', 'LDCLRALB', 'LDCLRALH', 'LDCLRB', 'LDCLRH', 'LDCLRL',
  'LDCLRLB', 'LDCLRLH', 'LDEOR', 'LDEORA', 'LDEORAB', 'LDEORAH', 'LDEORAL',
  'LDEORALB', 'LDEORALH', 'LDEORB', 'LDEORH', 'LDEORL', 'LDEORLB', 'LDEORLH',
  'LDG', 'LDGV', 'LDLAR', 'LDLARB', 'LDLARH', 'LDM', 'LDMDA', 'LDMDB', 'LDMIA',
  'LDMIB', 'LDNP', 'LDP', 'LDPSW', 'LDR', 'LDRAA', 'LDRAB', 'LDRB', 'LDREX',
  'LDREXB', 'LDREXD', 'LDREXH', 'LDRH', 'LDRSB', 'LDRSH', 'LDRSW', 'LDSET',
  'LDSETA', 'LDSETAB', 'LDSETAH', 'LDSETAL', 'LDSETALB', 'LDSETALH', 'LDSETB',
  'LDSETH', 'LDSETL', 'LDSETLB', 'LDSETLH', 'LDSMAX', 'LDSMAXA', 'LDSMAXAB',
  'LDSMAXAH', 'LDSMAXAL', 'LDSMAXALB', 'LDSMAXALH', 'LDSMAXB', 'LDSMAXH',
  'LDSMAXL', 'LDSMAXLB', 'LDSMAXLH', 'LDSMIN', 'LDSMINA', 'LDSMINAB',
  'LDSMINAH', 'LDSMINAL', 'LDSMINALB', 'LDSMINALH', 'LDSMINB', 'LDSMINH',
  'LDSMINL', 'LDSMINLB', 'LDSMINLH', 'LDTR', 'LDTRB', 'LDTRH', 'LDTRSB',
  'LDTRSH', 'LDTRSW', 'LDUMAX', 'LDUMAXA', 'LDUMAXAB', 'LDUMAXAH', 'LDUMAXAL',
  'LDUMAXALB', 'LDUMAXALH', 'LDUMAXB', 'LDUMAXH', 'LDUMAXL', 'LDUMAXLB',
  'LDUMAXLH', 'LDUMIN', 'LDUMINA', 'LDUMINAB', 'LDUMINAH', 'LDUMINAL',
  'LDUMINALB', 'LDUMINALH', 'LDUMINB', 'LDUMINH', 'LDUMINL', 'LDUMINLB',
  'LDUMINLH', 'LDUR', 'LDURB', 'LDURH', 'LDURSB', 'LDURSH', 'LDURSW', 'LDXP',
  'LDXR', 'LDXRB', 'LDXRH', 'LSL', 'LSLS', 'LSLV', 'LSR', 'LSRS', 'LSRV',
  'MADD', 'MCR', 'MCR2', 'MCRR', 'MCRR2', 'MLA', 'MLAS', 'MLS', 'MNEG', 'MOV',
  'MOVI', 'MOVK', 'MOVN', 'MOVS', 'MOVT', 'MOVZ', 'MRC', 'MRC2', 'MRRC',
  'MRRC2', 'MSUB', 'MUL', 'MULS', 'MVN', 'MVNI', 'MVNS', 'NEG',
  'NEGS', 'NGC', 'NGCS', 'NOT', 'ORN', 'ORNS', 'ORR', 'ORRS', 'PACDA',
  'PACDB', 'PACDZA', 'PACDZB', 'PACGA', 'PACIA', 'PACIA1716', 'PACIASP',
  'PACIAZ', 'PACIB', 'PACIB1716', 'PACIBSP', 'PACIBZ', 'PACIZA', 'PACIZB',
  'PKHBT', 'PKHTB', 'PLD', 'PLDW', 'PLI', 'PMUL', 'PMULL', 'PMULL2', 'POP',
  'PRFM', 'PRFUM', 'PSB', 'PUSH', 'QADD', 'QADD16', 'QADD8', 'QASX', 'QDADD',
  'QDSUB', 'QSAX', 'QSUB', 'QSUB16', 'QSUB8', 'RADDHN', 'RADDHN2', 'RAX1',
  'RBIT', 'REV', 'REV16', 'REV32', 'REV64', 'REVSH', 'ROR', 'RORS', 'RORV',
  'RRX', 'RRXS', 'RSB', 'RSBS', 'RSC', 'RSCS', 'RSHRN', 'RSHRN2', 'RSUBHN',
  'RSUBHN2', 'SABA', 'SABAL', 'SABAL2', 'SABD', 'SABDL', 'SABDL2', 'SADALP',
  'SADD16', 'SADD8', 'SADDL', 'SADDL2', 'SADDLP', 'SADDLV', 'SADDW', 'SADDW2',
  'SASX', 'SBC', 'SBCS', 'SBFIZ', 'SBFM', 'SBFX', 'SCVTF', 'SDIV', 'SDOT',
  'SEL', 'SETEND', 'SETPAN', 'SEV', 'SEVL', 'SG', 'SHA1C', 'SHA1H', 'SHA1M',
  'SHA1P', 'SHA1SU0', 'SHA1SU1', 'SHA256H', 'SHA256H2', 'SHA256SU0',
  'SHA256SU1', 'SHA512H', 'SHA512H2', 'SHA512SU0', 'SHA512SU1', 'SHADD',
  'SHADD16', 'SHADD8', 'SHASX', 'SHL', 'SHLL', 'SHLL2', 'SHRN', 'SHRN2',
  'SHSAX', 'SHSUB', 'SHSUB16', 'SHSUB8', 'SLI', 'SM3PARTW1', 'SM3PARTW2',
  'SM3SS1', 'SM3TT1A', 'SM3TT1B', 'SM3TT2A', 'SM3TT2B', 'SM4E', 'SM4EKEY',
  'SMADDL', 'SMAX', 'SMAXP', 'SMAXV', 'SMC', 'SMIN', 'SMINP', 'SMINV',
  'SMLABB', 'SMLABT', 'SMLAD', 'SMLADX', 'SMLAL', 'SMLAL2', 'SMLALBB',
  'SMLALBT', 'SMLALD', 'SMLALDX', 'SMLALS', 'SMLALTB', 'SMLALTT', 'SMLATB',
  'SMLATT', 'SMLAWB', 'SMLAWT', 'SMLSD', 'SMLSDX', 'SMLSL', 'SMLSL2', 'SMLSLD',
  'SMLSLDX', 'SMMLA', 'SMMLAR', 'SMMLS', 'SMMLSR', 'SMMUL', 'SMMULR', 'SMNEGL',
  'SMOV', 'SMSUBL', 'SMUAD', 'SMUADX', 'SMULBB', 'SMULBT', 'SMULH', 'SMULL',
  'SMULL2', 'SMULLS', 'SMULTB', 'SMULTT', 'SMULWB', 'SMULWT', 'SMUSD',
  'SMUSDX', 'SQABS', 'SQADD', 'SQDMLAL', 'SQDMLAL2', 'SQDMLSL', 'SQDMLSL2',
  'SQDMULH', 'SQDMULL', 'SQDMULL2', 'SQNEG', 'SQRDMLAH', 'SQRDMLSH',
  'SQRDMULH', 'SQRSHL', 'SQRSHRN', 'SQRSHRN2', 'SQRSHRUN', 'SQRSHRUN2',
  'SQSHL', 'SQSHLU', 'SQSHRN', 'SQSHRN2', 'SQSHRUN', 'SQSHRUN2', 'SQSUB',
  'SQXTN', 'SQXTN2', 'SQXTUN', 'SQXTUN2', 'SRHADD', 'SRI', 'SRS', 'SRSDA',
  'SRSDB', 'SRSHL', 'SRSHR', 'SRSIA', 'SRSIB', 'SRSRA', 'SSAT', 'SSAT16',
  'SSAX', 'SSHL', 'SSHLL', 'SSHLL2', 'SSHR', 'SSRA', 'SSUB16', 'SSUB8',
  'SSUBL', 'SSUBL2', 'SSUBW', 'SSUBW2', 'ST1', 'ST2', 'ST2G', 'ST3', 'ST4',
  'STADD', 'STADDB', 'STADDH', 'STADDL', 'STADDLB', 'STADDLH', 'STC', 'STC2',
  'STC2L', 'STCL', 'STCLR', 'STCLRB', 'STCLRH', 'STCLRL', 'STCLRLB', 'STCLRLH',
  'STEOR', 'STEORB', 'STEORH', 'STEORL', 'STEORLB', 'STEORLH', 'STG', 'STGP',
  'STGV', 'STL', 'STLB', 'STLEX', 'STLEXB', 'STLEXD', 'STLEXH', 'STLH',
  'STLLR', 'STLLRB', 'STLLRH', 'STLR', 'STLRB', 'STLRH', 'STLXP', 'STLXR',
  'STLXRB', 'STLXRH', 'STM', 'STMDA', 'STMDB', 'STMIA', 'STMIB', 'STNP', 'STP',
  'STR', 'STRB', 'STREX', 'STREXB', 'STREXD', 'STREXH', 'STRH', 'STSET',
  'STSETB', 'STSETH', 'STSETL', 'STSETLB', 'STSETLH', 'STSMAX', 'STSMAXB',
  'STSMAXH', 'STSMAXL', 'STSMAXLB', 'STSMAXLH', 'STSMIN', 'STSMINB', 'STSMINH',
  'STSMINL', 'STSMINLB', 'STSMINLH', 'STTR', 'STTRB', 'STTRH', 'STUMAX',
  'STUMAXB', 'STUMAXH', 'STUMAXL', 'STUMAXLB', 'STUMAXLH', 'STUMIN', 'STUMINB',
  'STUMINH', 'STUMINL', 'STUMINLB', 'STUMINLH', 'STUR', 'STURB', 'STURH',
  'STXP', 'STXR', 'STXRB', 'STXRH', 'STZ2G', 'STZG', 'SUB', 'SUBG', 'SUBHN',
  'SUBHN2', 'SUBP', 'SUBPS', 'SUBS', 'SUQADD', 'SVC', 'SWP', 'SWPA', 'SWPAB',
  'SWPAH', 'SWPAL', 'SWPALB', 'SWPALH', 'SWPB', 'SWPH', 'SWPL', 'SWPLB',
  'SWPLH', 'SXTAB', 'SXTAB16', 'SXTAH', 'SXTB', 'SXTB16', 'SXTH', 'SXTL',
  'SXTL2', 'SXTW', 'TBB', 'TBH', 'TBL', 'TBX', 'TEQ',
  'TRN1', 'TRN2', 'TST', 'TT', 'TTA', 'TTAT', 'TTT', 'UABA', 'UABAL', 'UABAL2',
  'UABD', 'UABDL', 'UABDL2', 'UADALP', 'UADD16', 'UADD8', 'UADDL', 'UADDL2',
  'UADDLP', 'UADDLV', 'UADDW', 'UADDW2', 'UASX', 'UBFIZ', 'UBFM', 'UBFX',
  'UCVTF', 'UDF', 'UDIV', 'UDOT', 'UHADD', 'UHADD16', 'UHADD8', 'UHASX',
  'UHSAX', 'UHSUB', 'UHSUB16', 'UHSUB8', 'UMAAL', 'UMADDL', 'UMAX', 'UMAXP',
  'UMAXV', 'UMIN', 'UMINP', 'UMINV', 'UMLAL', 'UMLAL2', 'UMLALS', 'UMLSL',
  'UMLSL2', 'UMNEGL', 'UMOV', 'UMSUBL', 'UMULH', 'UMULL', 'UMULL2', 'UMULLS',
  'UQADD', 'UQADD16', 'UQADD8', 'UQASX', 'UQRSHL', 'UQRSHRN', 'UQRSHRN2',
  'UQSAX', 'UQSHL', 'UQSHRN', 'UQSHRN2', 'UQSUB', 'UQSUB16', 'UQSUB8', 'UQXTN',
  'UQXTN2', 'URECPE', 'URHADD', 'URSHL', 'URSHR', 'URSQRTE', 'URSRA', 'USAD8',
  'USADA8', 'USAT', 'USAT16', 'USAX', 'USHL', 'USHLL', 'USHLL2', 'USHR',
  'USQADD', 'USRA', 'USUB16', 'USUB8', 'USUBL', 'USUBL2', 'USUBW', 'USUBW2',
  'UXTAB', 'UXTAB16', 'UXTAH', 'UXTB', 'UXTB16', 'UXTH', 'UXTL', 'UXTL2',
  'UZP1', 'UZP2', 'VABA', 'VABAL', 'VABD', 'VABDL', 'VABS', 'VACGE', 'VACGT',
  'VACLE', 'VACLT', 'VADD', 'VADDHN', 'VADDL', 'VADDW', 'VAND', 'VBIC', 'VBIF',
  'VBIT', 'VBSL', 'VCADD', 'VCEQ', 'VCGE', 'VCGT', 'VCLE', 'VCLS', 'VCLT',
  'VCLZ', 'VCMLA', 'VCMP', 'VCMPE', 'VCNT', 'VCVT', 'VCVTA', 'VCVTB', 'VCVTM',
  'VCVTN', 'VCVTP', 'VCVTT', 'VDIV', 'VDUP', 'VEOR', 'VEXT', 'VFMA', 'VFMAL',
  'VFMS', 'VFMSL', 'VFNMA', 'VFNMS', 'VHADD', 'VHSUB', 'VJCVT', 'VLD1', 'VLD2',
  'VLD3', 'VLD4', 'VLDM', 'VLDMDB', 'VLDMEA', 'VLDMFD', 'VLDMIA', 'VLDR',
  'VLLDM', 'VLSTM', 'VMAX', 'VMAXNM', 'VMIN', 'VMINNM', 'VMLA', 'VMLAL',
  'VMLS', 'VMLSL', 'VMOV', 'VMOV2', 'VMOVL', 'VMOVN', 'VMRS', 'VMSR', 'VMUL',
  'VMULL', 'VMVN', 'VNEG', 'VNMLA', 'VNMLS', 'VNMUL', 'VORN', 'VORR', 'VPADAL',
  'VPADD', 'VPADDL', 'VPMAX', 'VPMIN', 'VPOP', 'VPUSH', 'VQABS', 'VQADD',
  'VQDMLAL', 'VQDMLSL', 'VQDMULH', 'VQDMULL', 'VQMOVN', 'VQMOVUN', 'VQNEG',
  'VQRDMULH', 'VQRSHL', 'VQRSHRN', 'VQRSHRUN', 'VQSHL', 'VQSHLU', 'VQSHRN',
  'VQSHRUN', 'VQSUB', 'VRADDHN', 'VRECPE', 'VRECPS', 'VREV16', 'VREV32',
  'VREV64', 'VRHADD', 'VRINTA', 'VRINTM', 'VRINTN', 'VRINTP', 'VRINTR',
  'VRINTX', 'VRINTZ', 'VRSHL', 'VRSHR', 'VRSHRN', 'VRSQRTE', 'VRSQRTS',
  'VRSRA', 'VRSUBHN', 'VSDOT', 'VSEL', 'VSHL', 'VSHLL', 'VSHR', 'VSHRN',
  'VSLI', 'VSQRT', 'VSRA', 'VSRI', 'VST1', 'VST2', 'VST3', 'VST4', 'VSTM',
  'VSTMDB', 'VSTMEA', 'VSTMFD', 'VSTMIA', 'VSTR', 'VSUB', 'VSUBHN', 'VSUBL',
  'VSUBW', 'VSWP', 'VTBL', 'VTBX', 'VTRN', 'VTST', 'VUDOT', 'VUZP', 'VZIP',
  'WFE', 'WFI', 'XAR', 'XPACD', 'XPACI', 'XPACLRI', 'XTN', 'XTN2', 'YIELD',
  'ZIP1', 'ZIP2');

const ArmJmpBuff: array[0..31] of string = (
  'B', 'BL', 'BLR', 'BLX', 'BLXNS', 'BLRAA', 'BLRAAZ', 'BLRAB', 'BLRABZ', 'BR',
  'BRAA', 'BRAAZ', 'BRAB', 'BRABZ', 'BTI', 'BX', 'BXNS', 'BXJ', 'CBNZ', 'CBZ',
  'TBNZ', 'TBZ', 'RET', 'RETAA', 'RETAB', 'ERET', 'ERETAA', 'ERETAB', 'RFEIA',
  'RFEIB', 'RFEDA', 'RFEDB');

const ArmKernelBuff: array[0..7] of string = (
  'AT', 'DC', 'IC', 'SYS', 'SYSL', 'TLBI', 'MRS', 'MSR');

const ArmDataTypeBuff: array[0..14] of string = (
  'I8', 'I16', 'I32', 'I64', 'S8', 'S16', 'S32', 'S64',
  'U8', 'U16', 'U32', 'U64', 'F16', 'F32', 'P8');

const ArmSuffixBuff: array[0..16] of string = (
  'EQ', 'NE', 'CS', 'HS', 'CC', 'LO', 'MI', 'PL',
  'VS', 'VC', 'HI', 'LS', 'GE', 'LT', 'GT', 'LE', 'AL');

const ArmBarrierBuff: array[0..12] of string = (
  'SY', 'LD', 'ST', 'ISH', 'ISHLD', 'ISHST', 'NSH', 'NSHLD',
  'NSHST', 'OSH', 'OSHLD', 'OSHST', '!');

const ArmNopBuff: array[0..0] of string = ('NOP');

const ArmRegsBuff: array[0..197] of string = (
  'W0', 'W1', 'W2', 'W3', 'W4', 'W5', 'W6', 'W7', 'W8', 'W9', 'W10', 'W11',
  'W12', 'W13', 'W14', 'W15', 'W16', 'W17', 'W18', 'W19', 'W20', 'W21', 'W22',
  'W23', 'W24', 'W25', 'W26', 'W27', 'W28', 'W29', 'W30',
  'R0', 'R1', 'R2', 'R3', 'R4', 'R5', 'R6', 'R7', 'R8', 'R9', 'R10', 'R11',
  'R12', 'R13', 'R14', 'R15', 'R16', 'R17', 'R18', 'R19', 'R20', 'R21', 'R22',
  'R23', 'R24', 'R25', 'R26', 'R27', 'R28', 'R29', 'R30',
  'X0', 'X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8', 'X9', 'X10', 'X11',
  'X12', 'X13', 'X14', 'X15', 'X16', 'X17', 'X18', 'X19', 'X20', 'X21', 'X22',
  'X23', 'X24', 'X25', 'X26', 'X27', 'X28', 'X29', 'X30',
  'S0', 'S1', 'S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'S8', 'S9', 'S10', 'S11',
  'S12', 'S13', 'S14', 'S15', 'S16', 'S17', 'S18', 'S19', 'S20', 'S21', 'S22',
  'S23', 'S24', 'S25', 'S26', 'S27', 'S28', 'S29', 'S30', 'S31',
  'D0', 'D1', 'D2', 'D3', 'D4', 'D5', 'D6', 'D7', 'D8', 'D9', 'D10', 'D11',
  'D12', 'D13', 'D14', 'D15', 'D16', 'D17', 'D18', 'D19', 'D20', 'D21', 'D22',
  'D23', 'D24', 'D25', 'D26', 'D27', 'D28', 'D29', 'D30', 'D31',
  'V0', 'V1', 'V2', 'V3', 'V4', 'V5', 'V6', 'V7', 'V8', 'V9', 'V10', 'V11',
  'V12', 'V13', 'V14', 'V15', 'V16', 'V17', 'V18', 'V19', 'V20', 'V21', 'V22',
  'V23', 'V24', 'V25', 'V26', 'V27', 'V28', 'V29', 'V30', 'V31',
  'FP', 'LR', 'SP', 'PC', 'CPSR', 'FPCR', 'FPSR', 'XZR', 'WZR'
  );

const ArmRegTypesBuff: array[0..197] of TRegType = (
  // W0..W30
  rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32,
  rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32,
  rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32,
  rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32,
  // R0..R30
  rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32,
  rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32,
  rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32,
  rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32,
  // X0..X30
  rtReg64, rtReg64, rtReg64, rtReg64, rtReg64, rtReg64, rtReg64, rtReg64,
  rtReg64, rtReg64, rtReg64, rtReg64, rtReg64, rtReg64, rtReg64, rtReg64,
  rtReg64, rtReg64, rtReg64, rtReg64, rtReg64, rtReg64, rtReg64, rtReg64,
  rtReg64, rtReg64, rtReg64, rtReg64, rtReg64, rtReg64, rtReg64,
  // S0..S31
  rtSingle, rtSingle, rtSingle, rtSingle, rtSingle, rtSingle, rtSingle,
  rtSingle, rtSingle, rtSingle, rtSingle, rtSingle, rtSingle, rtSingle,
  rtSingle, rtSingle, rtSingle, rtSingle, rtSingle, rtSingle, rtSingle,
  rtSingle, rtSingle, rtSingle, rtSingle, rtSingle, rtSingle, rtSingle,
  rtSingle, rtSingle, rtSingle, rtSingle,
  // D0..D31
  rtDouble, rtDouble, rtDouble, rtDouble, rtDouble, rtDouble, rtDouble,
  rtDouble, rtDouble, rtDouble, rtDouble, rtDouble, rtDouble, rtDouble,
  rtDouble, rtDouble, rtDouble, rtDouble, rtDouble, rtDouble, rtDouble,
  rtDouble, rtDouble, rtDouble, rtDouble, rtDouble, rtDouble, rtDouble,
  rtDouble, rtDouble, rtDouble, rtDouble,
  // V0..V31
  rtSimd128, rtSimd128, rtSimd128, rtSimd128, rtSimd128, rtSimd128, rtSimd128,
  rtSimd128, rtSimd128, rtSimd128, rtSimd128, rtSimd128, rtSimd128, rtSimd128,
  rtSimd128, rtSimd128, rtSimd128, rtSimd128, rtSimd128, rtSimd128, rtSimd128,
  rtSimd128, rtSimd128, rtSimd128, rtSimd128, rtSimd128, rtSimd128, rtSimd128,
  rtSimd128, rtSimd128, rtSimd128, rtSimd128,
  // FP     LR        SP        PC        CPSR     FPCR     FPSR
  rtRegPtr, rtRegPtr, rtRegPtr, rtRegPtr, rtReg32, rtReg32, rtReg32,
  // XZR   WZR
  rtReg64, rtReg32
  );

{ TAsmTokenizer }

constructor TAsmTokenizer.Create;
begin
  FTokens := TDictionary<string, TTokenType>.Create;
  UpdateMode;
end;

destructor TAsmTokenizer.Destroy;
begin
  FTokens.Free;
  inherited;
end;

function TAsmTokenizer.GetToken(pData: PChar;
  var TokenLength: Integer): TTokenType;

  function IsTokenChar(Value: PChar): Boolean;
  begin
    Result := False;
    case Value^ of
      'A'..'Z', 'a'..'z', '0'..'9', '$': Result := True;
      '%': Result := TokenizerMode = tmAtAntT;
      '.', '-', '#', '!': Result := TokenizerMode = tmArm;
    end;
  end;

var
  pCursor: PChar;
  FoundToken: string;
  CharLeft, I: Integer;
  FirstIsTokenChar: Boolean;
begin
  Result := ttUnknown;
  pCursor := pData;
  CharLeft := 0;
  FirstIsTokenChar := IsTokenChar(pCursor);
  while (FirstIsTokenChar = IsTokenChar(pCursor)) and (CharLeft < TokenLength) do
  begin
    Inc(pCursor);
    Inc(CharLeft);
  end;
  TokenLength := CharLeft;
  if not FirstIsTokenChar then
    Exit;
  FoundToken := Copy(pData, 1, CharLeft);
  if (TokenizerMode = tmAtAntT) and CharInSet(FoundToken[1], ['%', '$']) then
    FoundToken := Copy(FoundToken, 2, CharLeft - 1);
  if IsNumber(FoundToken) then
    Result := ttNumber
  else
    FTokens.TryGetValue(FoundToken, Result);
  if (Result = ttUnknown) and (TokenizerMode = tmArm) then
  begin
    for I := 0 to Length(ArmSuffixBuff) - 1 do
      if FoundToken.EndsWith('.' + ArmSuffixBuff[I]) then
      begin
        SetLength(FoundToken, Length(FoundToken) - Length(ArmSuffixBuff[I]) - 1);
        FTokens.TryGetValue(FoundToken, Result);
        Break;
      end;
  end;
  if pCursor^ = ' ' then
    Inc(TokenLength);
end;

function TAsmTokenizer.GetRegType(const AReg: string): TRegType;
var
  I: Integer;
begin
  Result := rtUnknown;
  if TokenizerMode = tmArm then
  begin
    for I := 0 to Length(ArmRegsBuff) - 1 do
      if AReg = ArmRegsBuff[I] then
        Exit(ArmRegTypesBuff[I]);
    Exit;
  end;
  for I := 0 to Length(RegsBuff) - 1 do
    if AReg = RegsBuff[I] then
      Exit(RegTypesBuff[I]);
end;

function TAsmTokenizer.IsNumber(const Value: string): Boolean;
var
  Tmp: Int64;
begin
  if TokenizerMode = tmArm then
  begin
    if Pos('X', Value) > 0 then
      Result := False
    else
      if Value[1] = '#' then
        Result := TryStrToInt64(Copy(Value, 2, Length(Value)), Tmp)
      else
        Result := TryStrToInt64(Value, Tmp);
  end
  else
    Result := TryStrToInt64(Value, Tmp);
end;

procedure TAsmTokenizer.SetTokenizerMode(AValue: TTokenizerMode);
begin
  if FTokenizerMode <> AValue then
  begin
    FTokenizerMode := AValue;
    UpdateMode;
  end;
end;

procedure TAsmTokenizer.UpdateMode;
var
  I: Integer;
begin
  FTokens.Clear;
  if TokenizerMode in [tmIntel, tmAtAntT] then
  begin
    for I := 0 to Length(InstBuff) - 1 do
      FTokens.Add(InstBuff[I], ttInstruction);
    for I := 0 to Length(SizeBuff) - 1 do
      FTokens.Add(SizeBuff[I], ttSize);
    for I := 0 to Length(RegsBuff) - 1 do
      FTokens.Add(RegsBuff[I], ttReg);
    for I := 0 to Length(PrefixBuff) - 1 do
      FTokens.Add(PrefixBuff[I], ttPrefix);
    for I := 0 to Length(JmpBuff) - 1 do
      FTokens.Add(JmpBuff[I], ttJmp);
    for I := 0 to Length(KernelBuff) - 1 do
      FTokens.Add(KernelBuff[I], ttKernel);
    for I := 0 to Length(NopBuff) - 1 do
      FTokens.Add(NopBuff[I], ttNop);
    // AT&T specific
    if TokenizerMode = tmAtAntT then
    begin
      for I := 0 to Length(AtAndTInstBuff) - 1 do
        FTokens.Add(AtAndTInstBuff[I], ttInstruction);
      for I := 0 to Length(AtAndTJmpBuff) - 1 do
        FTokens.Add(AtAndTJmpBuff[I], ttJmp);
      for I := 0 to Length(AtAndTPrefixBuff) - 1 do
        FTokens.Add(AtAndTPrefixBuff[I], ttJmp);
    end;
  end;
  if TokenizerMode = tmArm then
  begin
    for I := 0 to Length(ArmInstBuff) - 1 do
      FTokens.Add(ArmInstBuff[I], ttInstruction);
    for I := 0 to Length(ArmRegsBuff) - 1 do
      FTokens.Add(ArmRegsBuff[I], ttReg);
    for I := 0 to Length(ArmJmpBuff) - 1 do
      FTokens.Add(ArmJmpBuff[I], ttJmp);
    for I := 0 to Length(ArmKernelBuff) - 1 do
      FTokens.Add(ArmKernelBuff[I], ttKernel);
    for I := 0 to Length(ArmNopBuff) - 1 do
      FTokens.Add(ArmNopBuff[I], ttNop);
    for I := 0 to Length(ArmSuffixBuff) - 1 do
      FTokens.Add(ArmSuffixBuff[I], ttPrefix);
    //for I := 0 to Length(ArmDataTypeBuff) - 1 do
    //  FTokens.Add(ArmDataTypeBuff[I], ttSize);
    for I := 0 to Length(ArmBarrierBuff) - 1 do
      FTokens.Add(ArmBarrierBuff[I], ttPrefix);
  end;
end;

end.
