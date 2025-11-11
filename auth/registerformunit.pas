unit RegisterFormUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TRegister }

  TRegister = class(TForm)
    Register: TButton;
    Cancel: TButton;
    chkAcceptTerms: TCheckBox;
    edtEmail: TEdit;
    edtUsername: TEdit;
    edtPassword: TEdit;
    edtConfirmPassword: TEdit;
    lblRegistorFormTitle: TLabel;
    lblRegisterStatus: TLabel;
  private

  public

  end;

var
  RegisterForm: TRegister;

implementation

{$R *.lfm}

end.

