object ViewParse: TViewParse
  Left = 0
  Top = 0
  Caption = 'Parsers'
  ClientHeight = 287
  ClientWidth = 675
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ParsersGrid: TStringGrid
    Left = 10
    Top = 8
    Width = 657
    Height = 233
    DefaultRowHeight = 18
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
    TabOrder = 0
    ColWidths = (
      89
      79
      82
      82
      310)
    RowHeights = (
      18
      18)
  end
  object btnStop: TButton
    Left = 136
    Top = 254
    Width = 75
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 1
  end
  object btnStart: TButton
    Left = 24
    Top = 254
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 2
    OnClick = btnStartClick
  end
end
