use proc_macro::{Punct, Spacing, TokenStream, TokenTree};
use quote::quote;
use syn::{self, parse::Parser};

#[proc_macro_attribute]
pub fn tracker_drive_struct(_metadata: TokenStream, input: TokenStream) -> TokenStream {
    let mut item_struct: syn::ItemStruct = syn::parse(input).expect("struct");

    match &mut item_struct.fields {
        syn::Fields::Named(fields) => {
            fields.named.push(
                syn::Field::parse_named
                    .parse2(quote! {
                        tracking_stack: Vec<Vec<(String, std::time::Instant, Option<std::time::Duration>)>>
                    })
                    .expect("tracking stack field"),
            );
            fields.named.push(
                syn::Field::parse_named
                    .parse2(quote! {
                            curr_stack: Vec<(String, std::time::Instant, Option<std::time::Duration>)>
                    })
                    .expect("curr track stack field"),
            );

            return quote!(#item_struct).into();
        }
        _ => panic!("REEE"),
    }
}

#[proc_macro_attribute]
pub fn track(_metadata: TokenStream, input: TokenStream) -> TokenStream {
    let item_fn: syn::ItemFn = syn::parse(input).expect("function");
    let body = item_fn.block;
    let name = item_fn.sig.ident.clone();
    let sig = item_fn.sig;
    let vis = item_fn.vis;

    TokenStream::from(quote! {
        #vis #sig {
            self.curr_stack.push((stringify!(#name).to_string(), std::time::Instant::now(), None));
            self.tracking_stack.push(self.curr_stack.clone());
            let rt_val = (||  #body) ();

            // self.curr_stack.pop();
            let mut last = self.curr_stack.pop();
            if let Some(mut last) = last {
                last = (last.0, last.1, Some(last.1.elapsed()));
                self.curr_stack.push(last);
            };
            self.tracking_stack.push(self.curr_stack.clone());
            self.curr_stack.pop();
            // self.tracking_stack.push(self.curr_stack.clone());

            rt_val
        }

        // fn e (&self) {}

    })
    .into()
}
