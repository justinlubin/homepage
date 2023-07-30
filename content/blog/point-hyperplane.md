+++
title = "Point-hyperplane distance"
date = 2023-05-29
+++

Here’s a super simple proof I did that I thought was quite neat during the course of [CS 289A: Introduction to Machine Learning](https://people.eecs.berkeley.edu/~jrs/189/) at Berkeley!

**Theorem.** *The distance between a point $x \in \mathbb{R}^n$ and a hyperplane $H = \\{v \in \mathbb{R}^n \mid w \cdot v + \alpha = 0 \\}$ for $w \in \mathbb{R}^n$ and $\alpha \in \mathbb{R}$ is $\frac{|w\cdot x + \alpha|}{||w||}$.*

*Proof.* Suppose $\lambda\in \mathbb{R}$ such that $x - \lambda w \in H$. Then, as $w$ is normal to $H$, the distance between $x$ and $H$ is $||\lambda w||$. Moreover, as $x - \lambda w \in H$, we have

$$
0 = w\cdot (x - \lambda w )+ \alpha = w \cdot x - \lambda ||w||^2 + \alpha,
$$

so $\lambda =
\frac{w\cdot x + \alpha}{||w||^2}$. {{ qed() }}
