use crate::ast::*;

fn gen_function(name: String, args: Vec<String>, body: Statement) {
    println!("{}:\n", name);
}

pub fn generate(statements: Vec<RootStatement>) {
    for statement in statements {
        match statement {
            RootStatement::Function(name, args, body) => {
                gen_function(name, args, body);
            },
            //other => println!("Can't compile this yet: {:?}", other),
        }
    }
}
