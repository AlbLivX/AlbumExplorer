unit UserObjectUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, DB, Uni, dDatenbank;

const
  C_INI_FILE = 'user.ini';
  C_SECTION  = 'Login';

type
  { TUserObject: Encapsulates all user-related operations }
  TUserObject = class
  private
    FUsername: string;
    FEmail: string;
    FPassword: string;
    FStayLoggedIn: Boolean;
    FLastError: string;

    FQueryRegister: TUniQuery;
    FQueryLogin: TUniQuery;

    procedure SetPassword(const APassword: string);
    function FieldExists(const AField, AValue: string): Boolean; // Refactored duplicate logic
    procedure HandleDatabaseError(const E: Exception); // Centralized error handling
  public
    constructor Create(AQueryRegister, AQueryLogin: TUniQuery);

    property Username: string read FUsername write FUsername;
    property Email: string read FEmail write FEmail;
    property Password: string write SetPassword;
    property StayLoggedIn: Boolean read FStayLoggedIn write FStayLoggedIn;
    property LastError: string read FLastError;

    function ValidateCredentials(const AUsername, APassword: string): Boolean;
    function UsernameExists(const AUsername: string): Boolean;
    function EmailExists(const AEmail: string): Boolean;
    function RegisterUser: Boolean;

    procedure LoadSettings;
    procedure SaveSettings;
  end;

implementation

{ Constructor }
constructor TUserObject.Create(AQueryRegister, AQueryLogin: TUniQuery);
begin
  inherited Create;
  FUsername := '';
  FEmail := '';
  FPassword := '';
  FStayLoggedIn := False;
  FLastError := '';
  FQueryRegister := AQueryRegister;
  FQueryLogin := AQueryLogin;
end;

{ Password Setter }
procedure TUserObject.SetPassword(const APassword: string);
begin
  FPassword := APassword; // plain text for now, hash later
end;

{ Load settings from INI }
procedure TUserObject.LoadSettings;
var
  Sett: TIniFile;
begin
  Sett := TIniFile.Create(C_INI_FILE);
  try
    FUsername := Sett.ReadString(C_SECTION, 'Username', '');
    FStayLoggedIn := Sett.ReadBool(C_SECTION, 'StayLoggedIn', False);
  finally
    Sett.Free;
  end;
end;

{ Save settings to INI }
procedure TUserObject.SaveSettings;
var
  Sett: TIniFile;
begin
  Sett := TIniFile.Create(C_INI_FILE);
  try
    Sett.WriteString(C_SECTION, 'Username', FUsername);
    Sett.WriteBool(C_SECTION, 'StayLoggedIn', FStayLoggedIn);
  finally
    Sett.Free;
  end;
end;

{ Centralized error handling for DB-related operations }
procedure TUserObject.HandleDatabaseError(const E: Exception);
begin
  FLastError := 'DB error: ' + E.Message;
end;

{ Generalized method to check if a field (Username or Email) exists in DB }
function TUserObject.FieldExists(const AField, AValue: string): Boolean;
begin
  Result := False;
  FLastError := '';
  try
    FQueryRegister.Close;
    //FQueryRegister.SQL.Text := Format('SELECT ID FROM USERS WHERE TRIM(%s) = TRIM(:Value)', [AField]);
    FQueryRegister.ParamByName('Value').AsString := AValue;
    FQueryRegister.Open;

    Result := not FQueryRegister.IsEmpty;
    FQueryRegister.Close;
  except
    on E: Exception do
    begin
      HandleDatabaseError(E); // Call centralized error handling
      Result := True; // assume exists for safety
    end;
  end;
end;

{ Check if username exists in DB }
function TUserObject.UsernameExists(const AUsername: string): Boolean;
begin
  Result := FieldExists('USERNAME', AUsername); // Reusing generalized method
end;

{ Check if email exists in DB }
function TUserObject.EmailExists(const AEmail: string): Boolean;
begin
  Result := FieldExists('EMAIL', AEmail); // Reusing generalized method
end;

{ Register user in DB using ExecSQL }
function TUserObject.RegisterUser: Boolean;
var
  NewID: Integer;
begin
  Result := False;
  FLastError := '';
  try
    // Get next ID from generator
    dmMain.qNextUserID.Connection := dmMain.cDatenbank;
    dmMain.qNextUserID.Open;
    NewID := dmMain.qNextUserID.FieldByName('NEWID').AsInteger;
    dmMain.qNextUserID.Close;

    // Insert new user via ExecSQL
    FQueryRegister.Close;
    FQueryRegister.ParamByName('ID').AsInteger := NewID;
    FQueryRegister.ParamByName('USERNAME').AsString := Trim(FUsername);
    FQueryRegister.ParamByName('EMAIL').AsString := Trim(FEmail);
    FQueryRegister.ParamByName('PWDHASH').AsString := Trim(FPassword);
    FQueryRegister.ExecSQL;

    // Commit transaction
    dmMain.cDatenbank.Commit;

    Result := True;
  except
    on E: Exception do
      HandleDatabaseError(E); // Call centralized error handling
  end;
end;

{ Validate credentials }
function TUserObject.ValidateCredentials(const AUsername, APassword: string): Boolean;
var
  StoredHash: string;
begin
  Result := False;
  FLastError := '';

  try
    FQueryLogin.Close;
    FQueryLogin.ParamByName('USERNAME').AsString := AUsername;
    FQueryLogin.Open;

    if FQueryLogin.IsEmpty then
    begin
      FLastError := 'User not found';
      Exit;
    end;

    StoredHash := FQueryLogin.FieldByName('PWDHASH').AsString;

    if StoredHash = APassword then
      Result := True
    else
      FLastError := 'Invalid password';

  except
    on E: Exception do
      HandleDatabaseError(E); // Call centralized error handling
  end;
end;

end.

