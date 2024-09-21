use cpu::Cpu;

mod cpu;
mod isa;

fn main() {
    let env = env_logger::Env::default()
        .filter_or("MY_LOG_LEVEL", "trace")
        .write_style_or("MY_LOG_STYLE", "always");
    env_logger::init_from_env(env);

    log::info!("Starting Fairy Famicom emulator...");

    let mut cpu = Cpu::new();
    cpu.load_program(vec![0xa9, 0x05, 0x00]).unwrap();
    cpu.run().unwrap();
}
