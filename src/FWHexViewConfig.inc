{$IFDEF FPC}
  {$MODE Delphi}
  {$WARN 5024 off : Parameter "$1" not used}
  {$WARN 5091 off : Local variable "$1" of a managed type does not seem to be initialized}
  {$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
  {$MACRO ON}
    {$IF FPC_FULLVERSION >= 30301}
      {$DEFINE EXTENDED_RTL}
	    {$WARN 6060 off : Case statement does not handle all possible cases}
	  {$ENDIF}
  {$MACRO OFF}
  {-$DEFINE USE_PROFILER}
  {$IFDEF LCLGtk2}
    {$DEFINE USE_CAIRO}
  {$ENDIF}
{$ENDIF}   
