package com.example.something;

import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Inject;
import jakarta.transaction.Transactional;
import java.math.BigDecimal;
import java.util.Objects;

@ApplicationScoped
public class Kyghg003Service {

    // ===== COPYBOOK DTOs =====

    static class CtaConstantes {
        String ctaPrograma = "KYGHG003";
        String ctaKyghr003 = "KYGHR003";
        String ctaParr120000 = "120000- ";
        String ctaSep = "#";
        String ctaI = "I";
        String ctaE = "E";
        String ctaV = "V";
        String ctaCodPaisoalfE = "COD-PAISOALF-E";
        String ctaCodEntalfaE = "COD-ENTALFA-E";
        String ctaCodPersctpnE = "COD-PERSCTPN-E";
        String ctaCodOperacnE = "COD-OPERACN-E";
        String ctaCodCanalDvE = "COD-CANAL-DV-E";
        int ctn04 = 4;
        int ctn12 = 12;
        int ctn08 = 8;
        int ctnErr22003 = 22003;
    }

    static class Variables {
        int wsCodAvierror;
        String wsSqlcaErrQpejcaqa;
    }

    static class Qpipccab {
        String codPais;
        String codBanco;
        String codOficina;
        String codPuesto;
        String codCanal;
        String codMedio;
        String codAplcanal;
        String codIdioma;
        String codUsuario;
        String fecProceso;
        String codBancoOper;
        String codOficinaOper;

        String xtiAvierror;
        int codAvierror;
        String codModuloErr;
        String codParrafoErr;
        String codTablaErr;
        String codAccesoErr;
        int qnuSqlcodeErr;
        String desSqlcaErr;
    }

    static class Qpejcaqa {
        String codPais;
        String codBanco;
        String codOficina;
        String codPuesto;
        String codCanal;
        String codMedio;
        String codAplcanal;
        String codIdioma;
        String codUsuario;
        String fecProceso;
        String codBancoOper;
        String codOficinaOper;

        int codRetorno;
        int codAvierror;
        String codModuloErr;
        String codParrafoErr;
        String codTablaErr;
        String codAccesoErr;
        int qnuSqlcodeErr;
        String desSqlcaErr;
    }

    static class Kyght003Entrada {
        String c003CodPaisoalfE;
        String c003CodEntalfaE;
        String c003CodPersctpnE;
        String c003CodOperacnE;
        String c003CodCanalDvE;
    }

    static class Kyght003Salida {
        String c003DesOperacnS;
        String c003DesLimiteS;
        String c003DesPeriodicidadS;
        BigDecimal c003ImpLimiteS;
        String c003CodSigno1S;
        BigDecimal c003ImpDisponibleS;
        String c003CodSigno2S;
        String c003FecRenovacionS;
        String c003CodDiisoalfS;
    }

    static class Kyghca03 {
        String codPaisoalfE;
        String codEntalfaE;
        String codPersctpnE;
        String codOperacnE;
        String codCanalDvE;

        String desOperacnS;
        String desLimiteS;
        String desPeriodicidadS;
        BigDecimal impLimiteS;
        String codSigno1S;
        BigDecimal impDisponibleS;
        String codSigno2S;
        String fecRenovacionS;
        String codDiisoalfS;
    }

    // ===== Injected CALL target =====
    @Inject
    Kyghr003Service kyghr003Service;

    private final CtaConstantes cta = new CtaConstantes();
    private final Variables variables = new Variables();
    private final Qpipccab qpipccab = new Qpipccab();

    // ===== Public entry point =====
    @Transactional
    public void execute(Qpejcaqa qpejcaqa, Kyght003Entrada entrada, Kyght003Salida salida) {
        perform100000Inicio(qpejcaqa, entrada, salida);
        perform200000Proceso(entrada, salida);
        perform300000Fin(qpejcaqa);
    }

    // ===== Paragraphs =====

    private void perform100000Inicio(Qpejcaqa qpejcaqa, Kyght003Entrada entrada, Kyght003Salida salida) {
        variables.wsCodAvierror = 0;
        variables.wsSqlcaErrQpejcaqa = null;
        salida.c003DesOperacnS = null;
        perform110000InformarContexto(qpejcaqa);
        perform120000ValidacionDatos(qpejcaqa, entrada);
    }

    private void perform110000InformarContexto(Qpejcaqa qpejcaqa) {
        qpipccab.codPais = qpejcaqa.codPais;
        qpipccab.codBanco = qpejcaqa.codBanco;
        qpipccab.codOficina = qpejcaqa.codOficina;
        qpipccab.codPuesto = qpejcaqa.codPuesto;
        qpipccab.codCanal = qpejcaqa.codCanal;
        qpipccab.codMedio = qpejcaqa.codMedio;
        qpipccab.codAplcanal = qpejcaqa.codAplcanal;
        qpipccab.codIdioma = qpejcaqa.codIdioma;
        qpipccab.codUsuario = qpejcaqa.codUsuario;
        qpipccab.fecProceso = qpejcaqa.fecProceso;
        qpipccab.codBancoOper = qpejcaqa.codBancoOper;
        qpipccab.codOficinaOper = qpejcaqa.codOficinaOper;
    }

    private void perform120000ValidacionDatos(Qpejcaqa qpejcaqa, Kyght003Entrada e) {
        validateRequired(cta.ctaCodPaisoalfE, e.c003CodPaisoalfE);
        validateRequired(cta.ctaCodEntalfaE, e.c003CodEntalfaE);
        validateRequired(cta.ctaCodPersctpnE, e.c003CodPersctpnE);
        validateRequired(cta.ctaCodOperacnE, e.c003CodOperacnE);

        if (isBlank(e.c003CodCanalDvE)) {
            e.c003CodCanalDvE = qpejcaqa.codCanal;
        }
    }

    private void perform200000Proceso(Kyght003Entrada entrada, Kyght003Salida salida) {
        perform210000RutinaKyghr003(entrada, salida);
    }

    private void perform210000RutinaKyghr003(Kyght003Entrada entrada, Kyght003Salida salida) {
        Kyghca03 ca03 = new Kyghca03();
        ca03.codPaisoalfE = entrada.c003CodPaisoalfE;
        ca03.codEntalfaE = entrada.c003CodEntalfaE;
        ca03.codPersctpnE = entrada.c003CodPersctpnE;
        ca03.codOperacnE = entrada.c003CodOperacnE;
        ca03.codCanalDvE = entrada.c003CodCanalDvE;

        kyghr003Service.execute(qpipccab, ca03);

        if (isBlank(qpipccab.xtiAvierror) || Objects.equals(qpipccab.xtiAvierror, "0")) {
            salida.c003DesOperacnS = ca03.desOperacnS;
            salida.c003DesLimiteS = ca03.desLimiteS;
            salida.c003DesPeriodicidadS = ca03.desPeriodicidadS;
            salida.c003ImpLimiteS = ca03.impLimiteS;
            salida.c003CodSigno1S = ca03.codSigno1S;
            salida.c003ImpDisponibleS = ca03.impDisponibleS;
            salida.c003CodSigno2S = ca03.codSigno2S;
            salida.c003FecRenovacionS = ca03.fecRenovacionS;
            salida.c003CodDiisoalfS = ca03.codDiisoalfS;
        } else {
            perform999999FinError(qpejcaqaFromQpipccab());
        }
    }

    private void perform300000Fin(Qpejcaqa qpejcaqa) {
        perform310000InformarContexto(qpejcaqa);
    }

    private void perform310000InformarContexto(Qpejcaqa qpejcaqa) {
        qpejcaqa.codPais = qpipccab.codPais;
        qpejcaqa.codBanco = qpipccab.codBanco;
        qpejcaqa.codOficina = qpipccab.codOficina;
        qpejcaqa.codPuesto = qpipccab.codPuesto;
        qpejcaqa.codCanal = qpipccab.codCanal;
        qpejcaqa.codMedio = qpipccab.codMedio;
        qpejcaqa.codAplcanal = qpipccab.codAplcanal;
        qpejcaqa.codIdioma = qpipccab.codIdioma;
        qpejcaqa.codUsuario = qpipccab.codUsuario;
        qpejcaqa.fecProceso = qpipccab.fecProceso;
        qpejcaqa.codBancoOper = qpipccab.codBancoOper;
        qpejcaqa.codOficinaOper = qpipccab.codOficinaOper;
    }

    private void perform900000InformaErrValidacion(Qpejcaqa qpejcaqa) {
        qpipccab.xtiAvierror = cta.ctaV;
        qpipccab.codAvierror = variables.wsCodAvierror;
        qpipccab.codModuloErr = cta.ctaPrograma;
        qpipccab.codParrafoErr = cta.ctaParr120000;
        qpipccab.codTablaErr = null;
        qpipccab.codAccesoErr = null;
        qpipccab.desSqlcaErr = variables.wsSqlcaErrQpejcaqa;
        qpipccab.qnuSqlcodeErr = 0;
        perform999999FinError(qpejcaqa);
    }

    private void perform999999FinError(Qpejcaqa qpejcaqa) {
        if (Objects.equals(qpipccab.xtiAvierror, cta.ctaI)) {
            qpejcaqa.codRetorno = cta.ctn04;
        } else if (Objects.equals(qpipccab.xtiAvierror, cta.ctaE)) {
            qpejcaqa.codRetorno = cta.ctn12;
        } else {
            qpejcaqa.codRetorno = cta.ctn08;
        }

        qpejcaqa.codAvierror = qpipccab.codAvierror;
        qpejcaqa.codModuloErr = qpipccab.codModuloErr;
        qpejcaqa.codParrafoErr = qpipccab.codParrafoErr;
        qpejcaqa.codTablaErr = qpipccab.codTablaErr;
        qpejcaqa.codAccesoErr = qpipccab.codAccesoErr;
        qpejcaqa.qnuSqlcodeErr = qpipccab.qnuSqlcodeErr;
        qpejcaqa.desSqlcaErr = qpipccab.desSqlcaErr;
    }

    // ===== Helpers =====

    private void validateRequired(String fieldName, String value) {
        if (isBlank(value)) {
            variables.wsSqlcaErrQpejcaqa = fieldName + value + cta.ctaSep;
            variables.wsCodAvierror = cta.ctnErr22003;
            perform900000InformaErrValidacion(qpejcaqaFromQpipccab());
        }
    }

    private boolean isBlank(String s) {
        return s == null || s.trim().isEmpty();
    }

    private Qpejcaqa qpejcaqaFromQpipccab() {
        return new Qpejcaqa();
    }
}

// ===== CALL TARGET INTERFACE =====
interface Kyghr003Service {
    void execute(Kyghg003Service.Qpipccab qpipccab, Kyghg003Service.Kyghca03 ca03);
}