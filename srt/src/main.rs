use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use subparse::SrtFile;
use subparse::SubtitleFileInterface;

fn read_file(path: &str) -> std::io::Result<SrtFile> {
    let file = File::open(path)?;
    let mut buf_reader = BufReader::new(file);
    let mut contents = String::new();
    buf_reader.read_to_string(&mut contents)?;
    Ok(SrtFile::parse(&contents).unwrap())
}

fn main() -> std::io::Result<()> {
    let original = read_file("../sui1_pek2_zep1_tu2_cet2_kaik.srt")?
        .get_subtitle_entries()
        .unwrap();
    let original2 = read_file("../sui1_pek2_zep1_tu2_cet2_kaik_INLIT_zep1_tet.srt")?
        .get_subtitle_entries()
        .unwrap();

    let mut iter = original2.iter();

    let mut ans = vec![];
    for subparse::SubtitleEntry { timespan, line } in original {
        let a = iter.next().unwrap().clone();
        if a.timespan != timespan {
            panic!("TIME MISMATCH")
        }
        ans.push((
            timespan,
            format!("{} / {}", line.unwrap(), a.line.as_ref().unwrap()),
        ));
    }
    let buf = SrtFile::create(ans).unwrap().to_data().unwrap();

    let mut file = File::create("../sui1_pek2_zep1_tu2_cet2_kaik_INLIT_zep1.srt")?;
    file.write_all(&buf)?;
    file.flush()?;
    Ok(())
}
