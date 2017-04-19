object frmJSONEdit: TfrmJSONEdit
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'JSON Edit'
  ClientHeight = 94
  ClientWidth = 314
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
  object lblKey: TLabel
    Left = 8
    Top = 11
    Width = 22
    Height = 13
    Caption = 'Key:'
  end
  object lblValue: TLabel
    Left = 8
    Top = 38
    Width = 30
    Height = 13
    Caption = 'Value:'
  end
  object edtKey: TEdit
    Left = 48
    Top = 8
    Width = 257
    Height = 21
    Enabled = False
    TabOrder = 0
    Text = 'edtKey'
  end
  object edtValue: TEdit
    Left = 48
    Top = 35
    Width = 257
    Height = 21
    TabOrder = 1
    Text = 'edtValue'
  end
  object btnSave: TBitBtn
    Left = 168
    Top = 62
    Width = 65
    Height = 25
    Caption = 'Save'
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TBitBtn
    Left = 247
    Top = 62
    Width = 58
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
