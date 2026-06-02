namespace CobolMigration.Kyghg003;

using System;
using System.Text;
using System.Threading.Tasks;

public sealed class Kyghg003Service
{
    private readonly IKyghr003Service _kyghr003;

    // === WORKING-STORAGE ===
    private CTAConstantes CTA_CONSTANTES = new();
    private Variables VARIABLES = new();

    // === COPYBOOK AREAS ===
    private QpipccabStub RETORNO_QPIPCCAB = new();
    private Ca03Kyghca03 CA03_KYGHCA03 = new();

    public Kyghg003Service(IKyghr003Service kyghr003)
    {
        _kyghr003 = kyghr003;
    }

    // === ENTRY POINT (PROCEDURE DIVISION) ===
    public async Task ExecuteAsync(
        QpejcaqaStub rQpejcaqa,
        Kyght003010101E entrada,
        Kyght003010101S salida)
    {
        await Perform100000Inicio(rQpejcaqa, entrada, salida);
        await Perform200000Proceso(rQpejcaqa, entrada, salida);
        await Perform300000Fin(rQpejcaqa);
    }

    // === 100000-INICIO ===
    private async Task Perform100000Inicio(
        QpejcaqaStub rQpejcaqa,
        Kyght003010101E entrada,
        Kyght003010101S salida)
    {
        RETORNO_QPIPCCAB = new();
        VARIABLES = new();
        salida.Clear();

        await Perform110000InformarContexto(rQpejcaqa);
        await Perform120000ValidacionDatos(rQpejcaqa, entrada);
    }

    // === 110000-INFORMAR-CONTEXTO ===
    private Task Perform110000InformarContexto(QpejcaqaStub ctx)
    {
        RETORNO_QPIPCCAB.CodPais = ctx.CodPais;
        RETORNO_QPIPCCAB.CodBanco = ctx.CodBanco;
        RETORNO_QPIPCCAB.CodOficina = ctx.CodOficina;
        RETORNO_QPIPCCAB.CodPuesto = ctx.CodPuesto;
        RETORNO_QPIPCCAB.CodCanal = ctx.CodCanal;
        RETORNO_QPIPCCAB.CodMedio = ctx.CodMedio;
        RETORNO_QPIPCCAB.CodAplCanal = ctx.CodAplCanal;
        RETORNO_QPIPCCAB.CodIdioma = ctx.CodIdioma;
        RETORNO_QPIPCCAB.CodUsuario = ctx.CodUsuario;
        RETORNO_QPIPCCAB.FecProceso = ctx.FecProceso;
        RETORNO_QPIPCCAB.CodBancoOper = ctx.CodBancoOper;
        RETORNO_QPIPCCAB.CodOficinaOper = ctx.CodOficinaOper;
        RETORNO_QPIPCCAB.CodPrograma = CTA_CONSTANTES.CTA_PROGRAMA;
        return Task.CompletedTask;
    }

    // === 120000-VALIDACION-DATOS ===
    private Task Perform120000ValidacionDatos(
        QpejcaqaStub ctx,
        Kyght003010101E entrada)
    {
        ValidateRequired(
            entrada.C003_COD_PAISOALF_E,
            CTA_CONSTANTES.CTA_COD_PAISOALF_E,
            entrada);

        ValidateRequired(
            entrada.C003_COD_ENTALFA_E,
            CTA_CONSTANTES.CTA_COD_ENTALFA_E,
            entrada);

        ValidateRequired(
            entrada.C003_COD_PERSCTPN_E,
            CTA_CONSTANTES.CTA_COD_PERSCTPN_E,
            entrada);

        ValidateRequired(
            entrada.C003_COD_OPERACN_E,
            CTA_CONSTANTES.CTA_COD_OPERACN_E,
            entrada);

        if (string.IsNullOrWhiteSpace(entrada.C003_COD_CANAL_DV_E) ||
            entrada.C003_COD_CANAL_DV_E.Length < 2)
        {
            entrada.C003_COD_CANAL_DV_E = ctx.CodCanal;
        }

        return Task.CompletedTask;
    }

    private void ValidateRequired(string value, string fieldName, Kyght003010101E entrada)
    {
        if (string.IsNullOrWhiteSpace(value))
        {
            VARIABLES.WS_COD_AVIERROR = CTA_CONSTANTES.CTN_ERR_22003;
            VARIABLES.WS_SQLCA_ERR_QPEJCAQA =
                $"{fieldName}{CTA_CONSTANTES.CTA_SEP}{value}{CTA_CONSTANTES.CTA_SEP}{entrada}";
            Perform900000InformaErrValidacion();
        }
    }

    // === 200000-PROCESO ===
    private async Task Perform200000Proceso(
        QpejcaqaStub ctx,
        Kyght003010101E entrada,
        Kyght003010101S salida)
    {
        await Perform210000RutinaKyghr003(ctx, entrada, salida);
    }

    // === 210000-RUTINA-KYGHR003 ===
    private async Task Perform210000RutinaKyghr003(
        QpejcaqaStub ctx,
        Kyght003010101E entrada,
        Kyght003010101S salida)
    {
        CA03_KYGHCA03 = new();
        CA03_KYGHCA03.Entrada.CA03_COD_PAISOALF_E = entrada.C003_COD_PAISOALF_E;
        CA03_KYGHCA03.Entrada.CA03_COD_ENTALFA_E = entrada.C003_COD_ENTALFA_E;
        CA03_KYGHCA03.Entrada.CA03_COD_PERSCTPN_E = entrada.C003_COD_PERSCTPN_E;
        CA03_KYGHCA03.Entrada.CA03_COD_OPERACN_E = entrada.C003_COD_OPERACN_E;
        CA03_KYGHCA03.Entrada.CA03_COD_CANAL_DV_E = entrada.C003_COD_CANAL_DV_E;

        await _kyghr003.ExecuteAsync(RETORNO_QPIPCCAB, CA03_KYGHCA03);

        if (string.IsNullOrWhiteSpace(RETORNO_QPIPCCAB.XtiAviError) ||
            RETORNO_QPIPCCAB.XtiAviError == "0")
        {
            salida.C003_DES_OPERACN_S = CA03_KYGHCA03.Salida.CA03_DES_OPERACN_S;
            salida.TB_C003_TABLA_LIMITES_S = CA03_KYGHCA03.Salida.TB_CA03_TABLA_LIMITES_S;
        }
        else
        {
            Perform999999FinError(ctx);
        }
    }

    // === 300000-FIN ===
    private Task Perform300000Fin(QpejcaqaStub ctx)
    {
        ctx.CodPais = RETORNO_QPIPCCAB.CodPais;
        ctx.CodBanco = RETORNO_QPIPCCAB.CodBanco;
        ctx.CodOficina = RETORNO_QPIPCCAB.CodOficina;
        ctx.CodPuesto = RETORNO_QPIPCCAB.CodPuesto;
        ctx.CodCanal = RETORNO_QPIPCCAB.CodCanal;
        ctx.CodMedio = RETORNO_QPIPCCAB.CodMedio;
        ctx.CodAplCanal = RETORNO_QPIPCCAB.CodAplCanal;
        ctx.CodIdioma = RETORNO_QPIPCCAB.CodIdioma;
        ctx.CodUsuario = RETORNO_QPIPCCAB.CodUsuario;
        ctx.FecProceso = RETORNO_QPIPCCAB.FecProceso;
        ctx.CodBancoOper = RETORNO_QPIPCCAB.CodBancoOper;
        ctx.CodOficinaOper = RETORNO_QPIPCCAB.CodOficinaOper;
        return Task.CompletedTask;
    }

    // === 900000-INFORMA-ERR-VALIDACION ===
    private void Perform900000InformaErrValidacion()
    {
        RETORNO_QPIPCCAB.XtiAviError = CTA_CONSTANTES.CTA_V;
        RETORNO_QPIPCCAB.CodAviError = VARIABLES.WS_COD_AVIERROR;
        RETORNO_QPIPCCAB.CodModuloErr = CTA_CONSTANTES.CTA_PROGRAMA;
        RETORNO_QPIPCCAB.CodParrafoErr = CTA_CONSTANTES.CTA_PARR_120000;
        RETORNO_QPIPCCAB.DesSqlcaErr = VARIABLES.WS_SQLCA_ERR_QPEJCAQA;
        Perform999999FinError(null);
    }

    // === 999999-FIN-ERROR ===
    private void Perform999999FinError(QpejcaqaStub ctx)
    {
        if (ctx == null) return;

        ctx.CodAviError = RETORNO_QPIPCCAB.CodAviError;
        ctx.CodModuloErr = RETORNO_QPIPCCAB.CodModuloErr;
        ctx.CodParrafoErr = RETORNO_QPIPCCAB.CodParrafoErr;
        ctx.DesSqlcaErr = RETORNO_QPIPCCAB.DesSqlcaErr;

        ctx.CodRetorno =
            RETORNO_QPIPCCAB.XtiAviError == CTA_CONSTANTES.CTA_I
                ? CTA_CONSTANTES.CTN_04
                : RETORNO_QPIPCCAB.XtiAviError == CTA_CONSTANTES.CTA_E
                    ? CTA_CONSTANTES.CTN_12
                    : CTA_CONSTANTES.CTN_08;
    }
}

// === CALLED PROGRAM INTERFACE ===
public interface IKyghr003Service
{
    Task ExecuteAsync(QpipccabStub header, Ca03Kyghca03 area);
}

// === DTOs ===
public record CTAConstantes
{
    public string CTA_PROGRAMA { get; init; } = "KYGHG003";
    public string CTA_PARR_120000 { get; init; } = "120000- ";
    public string CTA_SEP { get; init; } = "#";
    public string CTA_I { get; init; } = "I";
    public string CTA_E { get; init; } = "E";
    public string CTA_V { get; init; } = "V";
    public string CTA_COD_PAISOALF_E { get; init; } = "COD-PAISOALF-E";
    public string CTA_COD_ENTALFA_E { get; init; } = "COD-ENTALFA-E";
    public string CTA_COD_PERSCTPN_E { get; init; } = "COD-PERSCTPN-E";
    public string CTA_COD_OPERACN_E { get; init; } = "COD-OPERACN-E";
    public int CTN_04 { get; init; } = 4;
    public int CTN_08 { get; init; } = 8;
    public int CTN_12 { get; init; } = 12;
    public int CTN_ERR_22003 { get; init; } = 22003;
}

public record Variables
{
    public int WS_COD_AVIERROR { get; set; }
    public string WS_SQLCA_ERR_QPEJCAQA { get; set; } = string.Empty;
}

public record QpipccabStub
{
    public string CodPais { get; set; } = "";
    public string CodBanco { get; set; } = "";
    public string CodOficina { get; set; } = "";
    public string CodPuesto { get; set; } = "";
    public string CodCanal { get; set; } = "";
    public string CodMedio { get; set; } = "";
    public string CodAplCanal { get; set; } = "";
    public string CodIdioma { get; set; } = "";
    public string CodUsuario { get; set; } = "";
    public string FecProceso { get; set; } = "";
    public string CodBancoOper { get; set; } = "";
    public string CodOficinaOper { get; set; } = "";
    public string CodPrograma { get; set; } = "";
    public string XtiAviError { get; set; } = "";
    public int CodAviError { get; set; }
    public string CodModuloErr { get; set; } = "";
    public string CodParrafoErr { get; set; } = "";
    public string DesSqlcaErr { get; set; } = "";
}

public record QpejcaqaStub : QpipccabStub
{
    public int CodRetorno { get; set; }
}

public record Kyght003010101E
{
    public string C003_COD_PAISOALF_E { get; set; } = "";
    public string C003_COD_ENTALFA_E { get; set; } = "";
    public string C003_COD_PERSCTPN_E { get; set; } = "";
    public string C003_COD_OPERACN_E { get; set; } = "";
    public string C003_COD_CANAL_DV_E { get; set; } = "";
}

public record Kyght003010101S
{
    public string C003_DES_OPERACN_S { get; set; } = "";
    public object TB_C003_TABLA_LIMITES_S { get; set; } = new();
    public void Clear()
    {
        C003_DES_OPERACN_S = "";
        TB_C003_TABLA_LIMITES_S = new();
    }
}

public record Ca03Kyghca03
{
    public EntradaKyghca03 Entrada { get; set; } = new();
    public SalidaKyghca03 Salida { get; set; } = new();
}

public record EntradaKyghca03
{
    public string CA03_COD_PAISOALF_E { get; set; } = "";
    public string CA03_COD_ENTALFA_E { get; set; } = "";
    public string CA03_COD_PERSCTPN_E { get; set; } = "";
    public string CA03_COD_OPERACN_E { get; set; } = "";
    public string CA03_COD_CANAL_DV_E { get; set; } = "";
}

public record SalidaKyghca03
{
    public string CA03_DES_OPERACN_S { get; set; } = "";
    public object TB_CA03_TABLA_LIMITES_S { get; set; } = new();
}