unit LoginFormUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TLogin }

  TLogin = class(TForm)
    btnLogin: TButton;
    btnRegister: TButton;
    cbkStayLoggedIn: TCheckBox;
    edtUsername: TEdit;
    edtUserPassword: TEdit;
    lblLoginFormTitle: TLabel;
    lblLoginStatusMsg: TLabel;

  private

  public

  end;

var
  LoginForm: TLogin;

implementation

{$R *.lfm}

{ TLogin }

end.

