data{
    int brucellosi[5066];
    int age[5066];
    int province[5066];
    int year[5066];
}
parameters{
    vector[4] a;
    vector[4] b;
    vector[4] bA;
}
model{
    vector[5066] p;
    bA ~ normal( 0 , 1 );
    b ~ normal( 0 , 1 );
    a ~ normal( 0 , 1.5 );
    for ( i in 1:5066 ) {
        p[i] = a[year[i]] + b[year[i]] * province[i] + bA[year[i]] * age[i];
        // 
        p[i] = inv_logit(p[i]*0.995+(1-p[i])*(1-0.996));
    }
    brucellosi ~ binomial( 1 , p );
}
generated quantities{
    vector[5066] log_lik;
    vector[5066] p;
    for ( i in 1:5066 ) {
        p[i] = a[year[i]] + b[year[i]] * province[i] + bA[year[i]] * age[i];
        p[i] = inv_logit(p[i]*0.995+(1-p[i])*(1-0.996));
    }
    for ( i in 1:5066 ) log_lik[i] = binomial_lpmf( brucellosi[i] | 1 , p[i] );
}


































// 
// data{
//     int province[7357];
//     int year[7357];
//     int brucellosi[7357];
// }
// parameters{
//     real a;
//     
//  
// }
// model{
//     real p;
//     a ~ normal( 0 , 1.5 );
//     p = a;
//     p = inv_logit(p*0.995+(1-p)*(1-0.996));
//     // p =inv_logit(p);
//     brucellosi ~ binomial( 1 , p );
// }
// generated quantities{
//     vector[7357] log_lik;
//     real p;
//     p = a;
//     p = inv_logit(p);
//     for ( i in 1:7357 ) log_lik[i] = binomial_lpmf( brucellosi[i] | 1 , p );
// }



// data{
//     int brucellosi[7189];
//     int province[7189];
//     int year[7189];
// }
// parameters{
//     vector[6] a;
//     vector[6] b;
// }
// model{
//     vector[7189] p;
//     b ~ normal( 0 , 1 );
//     a ~ normal( 0 , 1.5 );
//     for ( i in 1:7189 ) {
//         p[i] = a[year[i]] + b[year[i]] * province[i];
//         p[i] = inv_logit(p[i]);
//     }
//     brucellosi ~ binomial( 1 , p );
// }
// generated quantities{
//     vector[7189] log_lik;
//     vector[7189] p;
//     for ( i in 1:7189 ) {
//         p[i] = a[year[i]] + b[year[i]] * province[i];
//         p[i] = inv_logit(p[i]*0.995+(1-p[i])*(1-0.996));
//     }
//     for ( i in 1:7189 ) log_lik[i] = binomial_lpmf( brucellosi[i] | 1 , p[i] );
// }

