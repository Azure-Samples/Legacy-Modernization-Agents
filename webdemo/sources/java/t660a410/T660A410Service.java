package com.example.something;

import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Inject;

@ApplicationScoped
public class T660A410Service {

    // ===== Working-Storage fields =====
    private String programNamn = "T660A410";
    private String wSection = "";

    private final String ja = "Y";
    private final String nej = "N";

    private String wArea = "";
    private int wAntalRader = 0;
    private String wAntalRaderZ = "";
    private int maxLa410RadIx = 1000;
    private int ackFnutt = 0;

    // ===== Injected CALL targets =====
    @Inject
    T6601010Service t6601010;

    @Inject
    T660A411Service t660a411;

    @Inject
    T6601112Service t6601112;

    // ===== Entry point (PROCEDURE DIVISION) =====
    public void execute(La410Area la410) {
        styr(la410);
    }

    // ===== STYR =====
    private void styr(La410Area la410) {
        wSection = "STYR";

        aInit(la410);
        bKollaBeh(la410);

        if (ja.equals(la410.w1010Flbehoerig)) {
            cKollaLa410(la410);
            if ("000".equals(la410.kdretur) && "0000".equals(la410.idfel)) {
                if ("05".equals(la410.kdbehand)) {
                    dHamtaVissaInt(la410);
                }
                fHamtaMedd(la410);
                gAntalRader(la410);
            } else {
                fHamtaMedd(la410);
            }
        } else {
            fHamtaMedd(la410);
        }

        zFinit();
    }

    // ===== A-INIT =====
    private void aInit(La410Area la410) {
        wSection = "A-";
        aaInitieraVariabler();
        la410.kdretur = "000";
        la410.idfel = "0000";
    }

    // ===== AA-INITIERA-VARIABLER =====
    private void aaInitieraVariabler() {
        wSection = "AA-INITIERA-VARIABLER";
        wArea = "";
        wAntalRader = 0;
        wAntalRaderZ = "";
        ackFnutt = 0;
    }

    // ===== B-KOLLA-BEH =====
    private void bKollaBeh(La410Area la410) {
        wSection = "B-";

        T6601010Dto dto = new T6601010Dto();
        dto.iduser = la410.iduser;
        dto.password = la410.password;
        dto.idba = la410.idba;

        t6601010.execute(dto, wArea);

        la410.idfel = dto.idfel;
        la410.kdretur = dto.kdretur;
        la410.w1010Flbehoerig = dto.flbehoerig;
        la410.w1010Kdspraak = dto.kdspraak;
    }

    // ===== C-KOLLA-LA410 =====
    private void cKollaLa410(La410Area la410) {
        wSection = "C-";
        if ("05".equals(la410.kdbehand)) {
            caKolla05(la410);
        } else {
            la410.idfel = "1006";
        }
    }

    // ===== CA-KOLLA-05 =====
    private void caKolla05(La410Area la410) {
        wSection = "CA-";

        if (la410.sortord == null || la410.sortord.trim().isEmpty()) {
            la410.kdsrtordIditemFrom = "01";
        }

        if (la410.idindapxIn == null || la410.idindapxIn.trim().isEmpty() || "%%%".equals(la410.idindapxIn)) {
            la410.idindapxIn = "VO ";
        }

        ackFnutt = 0;
        for (char c : la410.data.toCharArray()) {
            if (c == '\'') {
                ackFnutt++;
            }
        }
        if (ackFnutt > 0) {
            la410.idfel = "1104";
        }
    }

    // ===== D-HAMTA-VISSA-INT =====
    private void dHamtaVissaInt(La410Area la410) {
        wSection = "D-";

        T660A411Dto dto = new T660A411Dto();
        if (la410.idbaStart != null && !la410.idbaStart.trim().isEmpty()) {
            dto.iduser = "KOLA";
        }
        dto.fraga = la410.fraga;
        dto.kdbehand = la410.kdbehand;
        dto.kdbatch = la410.kdbatch;

        t660a411.execute(dto, wArea);

        la410.antal = dto.antal;
        la410.idfel = dto.idfel;
        la410.kdretur = dto.kdretur;
        la410.svar = dto.svar;
        la410.idservSqlfel = dto.idservSqlfel;
        la410.idsectSqlfel = dto.idsectSqlfel;
    }

    // ===== F-HAMTA-MEDD =====
    private void fHamtaMedd(La410Area la410) {
        wSection = "F-";

        T6601112Dto dto = new T6601112Dto();
        dto.idfelIn = la410.idfel;
        dto.kdspraak = la410.w1010Kdspraak;

        t6601112.execute(dto, wArea);

        if ("000".equals(dto.kdretur)) {
            la410.befel = dto.befelUt;
            la410.kdursPfel = dto.kdursPfelUt;
            la410.kdallvarFel = dto.kdallvarFelUt;
        }
    }

    // ===== G-ANTAL-RADER =====
    private void gAntalRader(La410Area la410) {
        wSection = "G-";

        wAntalRaderZ = la410.kvtraff;
        la410.befel = la410.befel.replace("XXXXXX", wAntalRaderZ);

        wAntalRaderZ = la410.kvradantal;
        la410.befel = la410.befel.replace("YYYYYY", wAntalRaderZ);
    }

    // ===== Z-FINIT =====
    private void zFinit() {
        wSection = "Z-";
    }
}

// ===== DTOs and service interfaces =====

class La410Area {
    public String iduser;
    public String password;
    public String idba;
    public String kdbehand;
    public String kdretur;
    public String idfel;
    public String sortord;
    public String kdsrtordIditemFrom;
    public String idindapxIn;
    public String data;
    public String idbaStart;
    public String fraga;
    public String kdbatch;
    public String antal;
    public String svar;
    public String idservSqlfel;
    public String idsectSqlfel;
    public String befel;
    public String kvtraff;
    public String kvradantal;

    // propagated from auth
    public String w1010Flbehoerig;
    public String w1010Kdspraak;
}

class T6601010Dto {
    public String iduser;
    public String password;
    public String idba;
    public String idfel;
    public String kdretur;
    public String flbehoerig;
    public String kdspraak;
}

class T660A411Dto {
    public String iduser;
    public String fraga;
    public String kdbehand;
    public String kdbatch;
    public String antal;
    public String idfel;
    public String kdretur;
    public String svar;
    public String idservSqlfel;
    public String idsectSqlfel;
}

class T6601112Dto {
    public String idfelIn;
    public String kdspraak;
    public String kdretur;
    public String befelUt;
    public String kdursPfelUt;
    public String kdallvarFelUt;
}

interface T6601010Service {
    void execute(T6601010Dto dto, String area);
}

interface T660A411Service {
    void execute(T660A411Dto dto, String area);
}

interface T6601112Service {
    void execute(T6601112Dto dto, String area);
}