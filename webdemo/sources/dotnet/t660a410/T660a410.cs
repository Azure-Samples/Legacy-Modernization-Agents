namespace CobolMigration.Something;

using System;
using System.Text;
using System.Threading.Tasks;

public sealed class T660A410Service
{
    private readonly IT6601010Service _t6601010;
    private readonly IT6601112Service _t6601112;
    private readonly IT660A411Service _t660A411;

    // WORKING-STORAGE
    private string ProgramNamn = "T660A410";
    private string WSection = string.Empty;

    private readonly GenerellaKonstanter Konstanter = new();
    private readonly DiverseVariabler Diverse = new();
    private readonly Subprogram Subprogram = new();

    public T660A410Service(
        IT6601010Service t6601010,
        IT6601112Service t6601112,
        IT660A411Service t660A411)
    {
        _t6601010 = t6601010;
        _t6601112 = t6601112;
        _t660A411 = t660A411;
    }

    // PROCEDURE DIVISION USING LA410-T660A410 LA410-AREA
    public async Task ExecuteAsync(LA410Area la410)
    {
        await Styr(la410);
    }

    // STYR SECTION
    private async Task Styr(LA410Area la410)
    {
        WSection = "STYR                         ";

        await AInit(la410);
        await BKollaBeh(la410);

        if (la410.W1010Flbehoerig == Konstanter.JA)
        {
            await CKollaLa410(la410);

            if (la410.Kdretur == "000" && la410.Idfel == "0000")
            {
                if (la410.Kdbehand == "05")
                {
                    await DHamtaVissaInt(la410);
                }

                await FHamtaMedd(la410);
                await GAntalRader(la410);
            }
            else
            {
                await FHamtaMedd(la410);
            }
        }
        else
        {
            await FHamtaMedd(la410);
        }

        await ZFinit();
    }

    // A-INIT SECTION
    private async Task AInit(LA410Area la410)
    {
        WSection = "A-                      ";
        await AAInitieraVariabler();

        la410.Kdretur = "000";
        la410.Idfel = "0000";
    }

    // AA-INITIERA-VARIABLER SECTION
    private Task AAInitieraVariabler()
    {
        WSection = "AA-INITIERA-VARIABLER   ";

        Diverse.WArea = string.Empty;
        Diverse.WAntalRader = 0;
        Diverse.WAntalRaderZ = 0;
        Diverse.AckFnutt = 0;

        return Task.CompletedTask;
    }

    // B-KOLLA-BEH SECTION
    private async Task BKollaBeh(LA410Area la410)
    {
        WSection = "B-                      ";

        var request = new T6601010Area
        {
            Iduser = la410.Iduser,
            Password = la410.Password,
            Idba = la410.Idba
        };

        var response = await _t6601010.ExecuteAsync(request, Diverse.WArea);

        la410.Idfel = response.Idfel;
        la410.Kdretur = response.Kdretur;
        la410.W1010Flbehoerig = response.Flbehoerig;
        la410.W1010Kdspraak = response.Kdspraak;
    }

    // C-KOLLA-LA410 SECTION
    private async Task CKollaLa410(LA410Area la410)
    {
        WSection = "C-                      ";

        if (la410.Kdbehand == "05")
        {
            await CAKolla05(la410);
        }
        else
        {
            la410.Idfel = "1006";
        }
    }

    // CA-KOLLA-05 SECTION
    private Task CAKolla05(LA410Area la410)
    {
        WSection = "CA-                     ";

        if (string.IsNullOrWhiteSpace(la410.Sortord))
        {
            la410.KdsrtordIditemFrom = "01";
        }

        if (string.IsNullOrWhiteSpace(la410.IdindapxIn) || la410.IdindapxIn == "%%%")
        {
            la410.IdindapxIn = "VO ";
        }

        Diverse.AckFnutt = 0;
        if (!string.IsNullOrEmpty(la410.Data))
        {
            foreach (var c in la410.Data)
            {
                if (c == '\'')
                    Diverse.AckFnutt++;
            }
        }

        if (Diverse.AckFnutt > 0)
        {
            la410.Idfel = "1104";
        }

        return Task.CompletedTask;
    }

    // D-HAMTA-VISSA-INT SECTION
    private async Task DHamtaVissaInt(LA410Area la410)
    {
        WSection = "D-                      ";

        var request = new T660A411Area
        {
            Iduser = la410.IdbaStart > " " ? "KOLA    " : string.Empty,
            Fraga = la410.Fraga,
            Kdbehand = la410.Kdbehand,
            Kdbatch = la410.Kdbatch
        };

        var response = await _t660A411.ExecuteAsync(request, Diverse.WArea);

        la410.Antal = response.Antal;
        la410.Idfel = response.Idfel;
        la410.Kdretur = response.Kdretur;
        la410.Svar = response.Svar;
        la410.IdservSqlfel = response.IdservSqlfel;
        la410.IdsectSqlfel = response.IdsectSqlfel;
    }

    // F-HAMTA-MEDD SECTION
    private async Task FHamtaMedd(LA410Area la410)
    {
        WSection = "F-                      ";

        var request = new T6601112Area
        {
            IdfelIn = la410.Idfel,
            Kdspraak = la410.W1010Kdspraak
        };

        var response = await _t6601112.ExecuteAsync(request, Diverse.WArea);

        if (response.Kdretur == "000")
        {
            la410.Befel = response.BefelUt;
            la410.KdurspFel = response.KdurspFelUt;
            la410.KdallvarFel = response.KdallvarFelUt;
        }
    }

    // G-ANTAL-RADER SECTION
    private Task GAntalRader(LA410Area la410)
    {
        WSection = "G-                      ";

        Diverse.WAntalRaderZ = la410.Kvtraff;
        if (!string.IsNullOrEmpty(la410.Befel))
        {
            la410.Befel = la410.Befel.Replace("XXXXXX", Diverse.WAntalRaderZ.ToString("000000"));
        }

        Diverse.WAntalRaderZ = la410.Kvradantal;
        if (!string.IsNullOrEmpty(la410.Befel))
        {
            la410.Befel = la410.Befel.Replace("YYYYYY", Diverse.WAntalRaderZ.ToString("000000"));
        }

        return Task.CompletedTask;
    }

    // Z-FINIT SECTION
    private Task ZFinit()
    {
        WSection = "Z-                      ";
        return Task.CompletedTask;
    }
}

// ================= DTOs =================

public sealed class GenerellaKonstanter
{
    public string JA { get; set; } = "Y";
    public string NEJ { get; set; } = "N";
}

public sealed class DiverseVariabler
{
    public string WArea { get; set; } = string.Empty;
    public int WAntalRader { get; set; }
    public int WAntalRaderZ { get; set; }
    public decimal MaxLa410RadIx { get; set; } = 1000;
    public int AckFnutt { get; set; }
}

public sealed class Subprogram
{
    public string T6601010 { get; set; } = "T6601010";
    public string T6601112 { get; set; } = "T6601112";
    public string T660A411 { get; set; } = "T660A411";
}

public sealed class LA410Area
{
    public string Iduser { get; set; } = string.Empty;
    public string Password { get; set; } = string.Empty;
    public string Idba { get; set; } = string.Empty;
    public string Kdbehand { get; set; } = string.Empty;
    public string Kdbatch { get; set; } = string.Empty;
    public string IdbaStart { get; set; } = string.Empty;
    public string Fraga { get; set; } = string.Empty;
    public string Sortord { get; set; } = string.Empty;
    public string KdsrtordIditemFrom { get; set; } = string.Empty;
    public string IdindapxIn { get; set; } = string.Empty;
    public string Data { get; set; } = string.Empty;

    public int Antal { get; set; }
    public string Svar { get; set; } = string.Empty;
    public int Kvtraff { get; set; }
    public int Kvradantal { get; set; }

    public string Befel { get; set; } = string.Empty;
    public string Idfel { get; set; } = string.Empty;
    public string Kdretur { get; set; } = string.Empty;

    public string IdservSqlfel { get; set; } = string.Empty;
    public string IdsectSqlfel { get; set; } = string.Empty;

    // Values returned from T6601010
    public string W1010Flbehoerig { get; set; } = string.Empty;
    public string W1010Kdspraak { get; set; } = string.Empty;

    // Message severity fields
    public string KdurspFel { get; set; } = string.Empty;
    public string KdallvarFel { get; set; } = string.Empty;
}

public sealed class WhenCompiled
{
    public string Value { get; set; } = string.Empty;
}

// ================= CALL TARGET INTERFACES =================

public interface IT6601010Service
{
    Task<T6601010Response> ExecuteAsync(T6601010Area request, string workArea);
}

public interface IT660A411Service
{
    Task<T660A411Response> ExecuteAsync(T660A411Area request, string workArea);
}

public interface IT6601112Service
{
    Task<T6601112Response> ExecuteAsync(T6601112Area request, string workArea);
}

// ================= CALL AREA DTOs =================

public sealed class T6601010Area
{
    public string Iduser { get; set; } = string.Empty;
    public string Password { get; set; } = string.Empty;
    public string Idba { get; set; } = string.Empty;
}

public sealed class T6601010Response
{
    public string Flbehoerig { get; set; } = string.Empty;
    public string Idfel { get; set; } = string.Empty;
    public string Kdretur { get; set; } = string.Empty;
    public string Kdspraak { get; set; } = string.Empty;
}

public sealed class T660A411Area
{
    public string Iduser { get; set; } = string.Empty;
    public string Fraga { get; set; } = string.Empty;
    public string Kdbehand { get; set; } = string.Empty;
    public string Kdbatch { get; set; } = string.Empty;
}

public sealed class T660A411Response
{
    public int Antal { get; set; }
    public string Idfel { get; set; } = string.Empty;
    public string Kdretur { get; set; } = string.Empty;
    public string Svar { get; set; } = string.Empty;
    public string IdservSqlfel { get; set; } = string.Empty;
    public string IdsectSqlfel { get; set; } = string.Empty;
}

public sealed class T6601112Area
{
    public string IdfelIn { get; set; } = string.Empty;
    public string Kdspraak { get; set; } = string.Empty;
}

public sealed class T6601112Response
{
    public string Kdretur { get; set; } = string.Empty;
    public string BefelUt { get; set; } = string.Empty;
    public string KdurspFelUt { get; set; } = string.Empty;
    public string KdallvarFelUt { get; set; } = string.Empty;
}